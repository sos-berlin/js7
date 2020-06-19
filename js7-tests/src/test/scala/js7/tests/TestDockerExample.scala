package js7.tests

import com.google.inject.Guice
import java.nio.file.Files.{createDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.process.ProcessSignal.SIGTERM
import js7.base.time.ScalaTime._
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.Closer.withCloser
import js7.base.utils.SideEffect.ImplicitSideEffect
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.log.Log4j
import js7.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.system.FileUtils.temporaryDirectory
import js7.common.utils.{JavaResource, JavaShutdownHook}
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.configuration.inject.ControllerModule
import js7.controller.tests.TestEnvironment
import js7.data.agent.AgentRefPath
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
object TestDockerExample
{
  private val TestAgentRefPaths = AgentRefPath("/agent-1") :: AgentRefPath("/agent-2") :: Nil

  def main(args: Array[String]) = {
    val directory =
      temporaryDirectory / "TestDockerExample" sideEffect { directory =>
        println(s"Using directory $directory")
        if (!Files.exists(directory))
          createDirectory(directory)
        else {
          println(s"Deleting $directory")
          deleteDirectoryContentRecursively(directory)
        }
      }
    try run(directory)
    finally Log4j.shutdown()
  }

  private def run(directory: Path): Unit = {
    val env = new TestEnvironment(TestAgentRefPaths, directory)
    def provide(path: String) = {
      val dir = if (path.startsWith("controller")) directory else env.agentsDir
      JavaResource(s"js7/install/docker/volumes/$path").copyToFile(dir / path)
      if (path contains "/executables/") setPosixFilePermissions(dir / path, PosixFilePermissions.fromString("rwx------"))
    }
    provide("controller/config/private/private.conf")
    provide("provider/config/live/მაგალითად.workflow.json")
    provide("provider/config/order-generators/test.order.xml")
    provide("agent-1/config/private/private.conf")
    provide("agent-1/config/executables/test")
    provide("agent-2/config/private/private.conf")
    provide("agent-2/config/executables/test")
    env.controllerDir / "config" / "controller.conf" := """js7.webserver.auth.loopback-is-public = on"""
    withCloser { implicit closer =>
      val controllerConfiguration = ControllerConfiguration.forTest(configAndData = env.controllerDir, httpPort = Some(4444))
      val injector = Guice.createInjector(new ControllerModule(controllerConfiguration.copy(
        config = controllerConfiguration.config)))
      injector.instance[Closer].closeWithCloser
      val agents = for (agentRefPath <- TestAgentRefPaths) yield {
        val agent = RunningAgent.startForTest(
          AgentConfiguration.forTest(configAndData = env.agentDir(agentRefPath))
        ) map { _.closeWithCloser } await 99.s
        //env.file(agentRefPath, SourceType.Json) := AgentRef(AgentRefPath.NoId, uri = agent.localUri.toString)
        agent
      }
      JavaShutdownHook.add("TestDockerExample") {
        print('\n')
        (for (agent <- agents) yield {
          agent.executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
          val r = agent.terminated
          agent.close()
          r
        }) await 10.s
        injector.instance[Closer].close()
        Log4j.shutdown()
      } .closeWithCloser

      val controller = RunningController.fromInjector(injector) await 99.s
      //??? controller.executeCommandAsSystemUser(ControllerCommand.ScheduleOrdersEvery(1.minute)).runToFuture.await(99.s).orThrow
      controller.terminated await 365 * 24.h
      controller.close()
      for (agent <- agents) agent.executeCommandAsSystemUser(AgentCommand.ShutDown())
      agents map (_.terminated) await 60.s
      agents foreach (_.close())
    }
  }
}
