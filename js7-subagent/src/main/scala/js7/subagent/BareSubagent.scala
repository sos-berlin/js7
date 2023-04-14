package js7.subagent

import cats.effect.concurrent.Deferred
import cats.effect.{Resource, Sync}
import java.nio.file.Path
import js7.base.auth.SimpleUser
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

object BareSubagent
{
  private val logger = Logger[this.type]

  type StartAsAgentDirector = StartAsAgentDirector.type
  object StartAsAgentDirector

  // Startable without a Scheduler
  def blockingRun(conf: SubagentConf, orCommonScheduler: Option[Scheduler] = None)
  : Either[StartAsAgentDirector, ProgramTermination] =
    threadPoolResource[Coeval](conf, orCommonScheduler)
      .use(implicit scheduler => Coeval {
        val restartAsDirectorVar = Deferred.unsafe[Task, Unit]
        val restartAsDirector = restartAsDirectorVar.complete(())
        resource(conf, scheduler, restartAsDirector)
          .use(bareSubagent => Task
            .race(
              restartAsDirectorVar.get,
              bareSubagent.untilTerminated)
            .flatMap {
              case Left(()) =>
                bareSubagent.shutdown(dontWaitForDirector = true)
                  .as(Left(StartAsAgentDirector))
              case Right(o) => Task.right(o)
            })
          .runSyncUnsafe()
      })
      .apply()

  def resource(
    conf: SubagentConf,
    js7Scheduler: Scheduler,
    restartAsDirector: Task[Unit] = Task.unit)
  : Resource[Task, Subagent] =
    Resource.suspend(Task {
      import conf.config
      implicit val s: Scheduler = js7Scheduler

      (for {
        iox <- IOExecutor.resource[Task](config, name = conf.name + "-I/O")
        // For BlockingInternalJob (thread-blocking Java jobs)
        testEventBus <- Resource.eval(Task(new StandardEventBus[Any]))
        subagent <- Subagent.resource(conf, js7Scheduler, iox, testEventBus)
        _ <- webServerResource(restartAsDirector, subagent, testEventBus, iox)
      } yield {
        logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
        subagent
      }).executeOn(js7Scheduler)
    })

  private def webServerResource(
    restartAsDirector: Task[Unit] = Task.unit,
    subagent: Subagent,
    testEventBus: StandardEventBus[Any],
    iox: IOExecutor)
    (implicit scheduler: Scheduler)
  : Resource[Task, AkkaWebServer] = {
    import subagent.conf
    import subagent.conf.config
    for {
      signatureVerifier <- DirectoryWatchingSignatureVerifier.prepare(config)
        .orThrow
        .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))(iox)
      commandExecutor = new SubagentCommandExecuter(subagent, signatureVerifier, subagent.journal)

      actorSystem <- Akkas.actorSystemResource(conf.name, config)
      sessionRegister = SessionRegister.start[SimpleSession](
        actorSystem, SimpleSession(_), config)
      _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
      webServer <- SubagentWebServer.resource(
        commandExecutor, subagent.journal, sessionRegister, restartAsDirector, conf)(actorSystem)
      _ <- provideUriFile(conf, webServer.localHttpUri)
    } yield webServer
  }

  private def provideUriFile(conf: SubagentConf, uri: Checked[Uri]): Resource[Task, Path] =
    provideFile[Task](conf.workDirectory / "http-uri")
      .evalTap(file => Task {
        for (uri <- uri) file := s"$uri/subagent"
      })

  def threadPoolResource[F[_]](conf: SubagentConf, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F])
  : Resource[F, Scheduler] =
    ThreadPools.standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
