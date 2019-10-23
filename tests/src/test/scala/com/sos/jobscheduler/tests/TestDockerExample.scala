package com.sos.jobscheduler.tests

import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Shutdown
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closer
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.utils.{JavaResource, JavaShutdownHook}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.tests.TestEnvironment
import java.nio.file.Files.{createDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.DurationInt

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
      val dir = if (path.startsWith("master")) directory else env.agentsDir
      JavaResource(s"com/sos/jobscheduler/install/docker/volumes/$path").copyToFile(dir / path)
      if (path contains "/executables/") setPosixFilePermissions(dir / path, PosixFilePermissions.fromString("rwx------"))
    }
    provide("master/config/private/private.conf")
    provide("provider/config/live/მაგალითად.workflow.json")
    provide("provider/config/order-generators/test.order.xml")
    provide("agent-1/config/private/private.conf")
    provide("agent-1/config/executables/test")
    provide("agent-2/config/private/private.conf")
    provide("agent-2/config/executables/test")
    env.masterDir / "config" / "master.conf" := """jobscheduler.webserver.auth.loopback-is-public = on"""
    withCloser { implicit closer =>
      val masterConfiguration = MasterConfiguration.forTest(configAndData = env.masterDir, httpPort = Some(4444))
      val injector = Guice.createInjector(new MasterModule(masterConfiguration.copy(
        config = masterConfiguration.config)))
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
          agent.executeCommand(Shutdown(sigtermProcesses = true, sigkillProcessesAfter = Some(3.seconds)))
          val r = agent.terminated
          agent.close()
          r
        }) await 10.s
        injector.instance[Closer].close()
        Log4j.shutdown()
      } .closeWithCloser

      val master = RunningMaster(injector) await 99.s
      //??? master.executeCommandAsSystemUser(MasterCommand.ScheduleOrdersEvery(1.minute)).runToFuture.await(99.s).orThrow
      master.terminated await 365 * 24.h
      master.close()
      for (agent <- agents) agent.executeCommand(AgentCommand.Shutdown())
      agents map (_.terminated) await 60.s
      agents foreach (_.close())
    }
  }
}
