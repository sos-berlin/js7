package js7.tests

import cats.effect.SyncIO
import cats.syntax.traverse.*
import java.nio.file.Files.{createDirectories, createDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, temporaryDirectory}
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Log4j
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.Closer.withCloser
import js7.base.utils.SideEffect.ImplicitSideEffect
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.common.utils.JavaShutdownHook
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.tests.TestEnvironment
import js7.data.agent.AgentPath
import monix.execution.Scheduler.Implicits.traced as scheduler

/**
  * @author Joacim Zschimmer
  */
object TestDockerExample
{
  private val TestAgentPaths = AgentPath("agent-1") :: AgentPath("agent-2") :: Nil

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
    val env = new TestEnvironment(TestAgentPaths, directory)
    def provide(path: String) = {
      val dir = if (path.startsWith("controller")) directory else env.agentsDir
      createDirectories((dir / path).getParent)
      JavaResource(s"js7/install/docker/volumes/$path").copyToFile(dir / path)
      if (path contains "/executables/") setPosixFilePermissions(dir / path, PosixFilePermissions.fromString("rwx------"))
    }
    provide("controller/config/private/private.conf")
    provide("provider/config/live/მაგალითად.workflow.json")
    provide("provider/config/order-generators/test.order.xml")
    provide("agent-a/config/private/private.conf")
    provide("agent-a/config/executables/test")
    provide("agent-b-1/config/private/private.conf")
    provide("agent-b-1/config/executables/exit-7")
    env.controllerDir / "config" / "controller.conf" := """js7.web.server.auth.loopback-is-public = on"""
    withCloser { implicit closer =>
      val conf = ControllerConfiguration.forTest(configAndData = env.controllerDir, httpPort = Some(4444))
      val agents = for (agentPath <- TestAgentPaths) yield {
        val agent = RunningAgent.startForTest(AgentConfiguration.forTest(
          configAndData = env.agentDir(agentPath),
          name = AgentConfiguration.DefaultName)
        ).map(_.closeWithCloser) await 99.s
        //env.file(agentPath, SourceType.Json) := AgentRef(AgentPath.NoId, uri = agent.localUri.toString)
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
        Log4j.shutdown()
      } .closeWithCloser

      RunningController.threadPoolResource[SyncIO](conf).useSync { implicit scheduler =>
        RunningController.resource(conf, scheduler)
          .blockingUse(99.s) { controller =>
            //??? controller.executeCommandAsSystemUser(ControllerCommand.ScheduleOrdersEvery(1.minute)).runToFuture.await(99.s).orThrow
            controller.terminated await 365 * 24.h
        }
        for (agent <- agents) agent.executeCommandAsSystemUser(AgentCommand.ShutDown())
        agents.traverse(_.terminated) await 60.s
        agents foreach (_.close())
      }
    }
  }
}
