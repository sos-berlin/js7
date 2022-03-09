package js7.tests

import cats.effect.Resource
import java.nio.file.Files.{createDirectories, createDirectory}
import java.nio.file.Path
import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.common.akkahttp.web.data.WebServerPort
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.subagent.SubagentRef
import js7.data.subagent.SubagentRefStateEvent.SubagentDedicated
import js7.proxy.ControllerApi
import js7.subagent.BareSubagent
import js7.subagent.configuration.SubagentConf
import js7.tests.SubagentTester._
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.control.NonFatal

trait SubagentTester
{
  this: DirectoryProviderForScalaTest =>

  protected val scheduler: Scheduler

  implicit private def implicitScheduler = scheduler

  protected lazy val controller: RunningController = directoryProvider
    .startController()
    .await(99.s)

  protected lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(Admission(
      controller.localUri,
      Some(directoryProvider.controller.userAndPassword)
    )))(controller.actorSystem))

  import controller.eventWatch

  protected final def startSubagentTester() =
    controller

  protected final def stopSubagentTester() = {
    controllerApi.stop.await(99.s)
    controller.terminate().await(99.s)
  }

  protected final def runSubagent[A](subagentRef: SubagentRef, awaitDedicated: Boolean = true)
    (body: BareSubagent => A)
  : Task[A] =
    Task.defer {
      val eventId = eventWatch.lastAddedEventId
      subagentResource(subagentRef).use { subagent =>
        if (awaitDedicated) eventWatch.await[SubagentDedicated](after = eventId)
        Task {
          try body(subagent)
          catch { case NonFatal(t) =>
            logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
            throw t
          }
        }
      }
    }

  protected final def subagentResource(subagentRef: SubagentRef): Resource[Task, BareSubagent] =
    for {
      dir <- subagentEnvironment(subagentRef)
      conf = toSubagentConf(dir, subagentRef.uri.port.orThrow, name = subagentRef.id.string)
      scheduler <- BareSubagent.threadPoolResource[Task](conf)
      subagent <- BareSubagent.resource(conf.finishAndProvideFiles, scheduler)
    } yield subagent

  protected final def subagentEnvironment(subagentRef: SubagentRef): Resource[Task, Path] =
    Resource.make(
      acquire = Task {
        val dir = directoryProvider.directory / "subagents" / subagentRef.id.string
        createDirectories(directoryProvider.directory / "subagents")
        createDirectory(dir)
        createDirectory(dir / "data")
        createDirectory(dir / "data" / "logs")
        dir
      })(
      release = dir => Task {
        deleteDirectoryRecursively(dir)
      })

  protected final def toSubagentConf(directory: Path, port: Int, name: String): SubagentConf =
    SubagentConf.of(
      configDirectory = directory / "config",
      dataDirectory = directory / "data",
      logDirectory = directory / "data" / "logs",
      jobWorkingDirectory = directory,
      Seq(WebServerPort.localhost(port)),
      killScript = None,
      name = s"SubagentSelectionTest-$name",
      config = config"""
        js7.job.execution.signed-script-injection-allowed = yes
        js7.auth.users.AGENT {
          permissions: [ AgentDirector ]
          password: "plain:AGENT-PASSWORD"
        }
        """
        .withFallback(SubagentConf.defaultConfig))
}

object SubagentTester {
  private val logger = Logger(getClass)
}
