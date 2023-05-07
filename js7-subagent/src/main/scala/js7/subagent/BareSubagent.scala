package js7.subagent

import cats.effect.concurrent.Deferred
import cats.effect.{Resource, Sync, SyncIO}
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
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.subagent.ConvertibleToDirector.{ConvertToDirector, convertibleToDirector}
import js7.subagent.Subagent.ItemSignatureKeysUpdated
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.Task
import monix.execution.Scheduler

object BareSubagent
{
  private val logger = Logger[this.type]

  def blockingRun(conf: SubagentConf, orCommonScheduler: Option[Scheduler] = None)
  : Either[ConvertToDirector, ProgramTermination] =
    threadPoolResource[SyncIO](conf, orCommonScheduler)
      .use(implicit scheduler => SyncIO {
        val convertToDirectorVar = Deferred.unsafe[Task, Unit]
        val convertToDirector = convertToDirectorVar.complete(()).attempt.void
        resource(conf, scheduler, convertToDirector)
          .use(bareSubagent => Task
            .race(
              convertToDirectorVar.get,
              bareSubagent.untilTerminated)
            .flatMap {
              case Left(()) =>
                bareSubagent.shutdown(dontWaitForDirector = true)
                  .as(Left(ConvertToDirector))
              case Right(o) => Task.right(o)
            })
          .runSyncUnsafe()
      })
      .unsafeRunSync()

  // ConvertibleToDirector requires an Allocated[…, Subagent]
  //def blockingRun(conf: SubagentConf): Either[ConvertToDirector, ProgramTermination] =
  //  convertibleToDirector(convertible =>
  //    ServiceMain.logging
  //      .blockingRun("Subagent", conf.config, convertible.resource(conf, _))(
  //        convertible.use(_)))

  def run(conf: SubagentConf)(implicit scheduler: Scheduler)
  : Task[Either[ConvertToDirector, ProgramTermination]] =
    Task.defer(
      convertibleToDirector(convertible =>
        convertible
          .resource(conf, scheduler)
          .toAllocated
          .flatMap(convertible.use)))

  def resource(
    conf: SubagentConf,
    js7Scheduler: Scheduler,
    convertToDirector: Task[Unit] = Task.unit)
  : Resource[Task, Subagent] =
    Resource.suspend(Task {
      import conf.config
      implicit val s: Scheduler = js7Scheduler

      (for {
        iox <- IOExecutor.resource[Task](config, name = conf.name + "-I/O")
        testEventBus <- Resource.eval(Task(new StandardEventBus[Any]))

        // Stop Subagent _after_ web service to allow last commands!
        subagentDeferred <- Resource.eval(Deferred[Task, Subagent])
        _ <- webServerResource(convertToDirector, subagentDeferred.get, conf, testEventBus, iox)
        subagent <- Subagent.resource(conf, js7Scheduler, iox, testEventBus)
        _ <- Resource.eval(subagentDeferred.complete(subagent))
      } yield {
        logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
        subagent
      }).executeOn(js7Scheduler)
    })

  private def webServerResource(
    convertToDirector: Task[Unit] = Task.unit,
    subagent: Task[Subagent],
    conf: SubagentConf,
    testEventBus: StandardEventBus[Any],
    iox: IOExecutor)
    (implicit scheduler: Scheduler)
  : Resource[Task, AkkaWebServer] = {
    import conf.config
    for {
      signatureVerifier <- DirectoryWatchingSignatureVerifier.prepare(config)
        .orThrow
        .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))(iox)
      actorSystem <- Akkas.actorSystemResource(conf.name, config)
      sessionRegister = SessionRegister.start[SimpleSession](
        actorSystem, SimpleSession(_), config)
      _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
      webServer <- SubagentWebServer.resource(
        commandExecutor = subagent.map(new SubagentCommandExecutor(_, signatureVerifier)).memoize,
        sessionRegister, convertToDirector, conf)(
        actorSystem, scheduler)
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
}
