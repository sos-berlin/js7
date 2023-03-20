package js7.subagent

import cats.effect.concurrent.Deferred
import cats.effect.{Resource, Sync}
import java.nio.file.Path
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.RichConfig
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.{newUnlimitedScheduler, schedulerServiceToResource}
import js7.data.subagent.SubagentCommand.ShutDown
import js7.data.subagent.{SubagentRunId, SubagentState}
import js7.journal.watch.InMemoryJournal
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly

final class BareSubagent private(
  private val commandExecutor: SubagentCommandExecutor,
  val journal: InMemoryJournal[SubagentState])
extends Service
{
  /** Completes when Subagent has stopped. */
  def untilTerminated: Task[ProgramTermination] =
    commandExecutor.untilTerminated

  protected def start =
    startService(commandExecutor.untilStopped.void)

  private val memoizedStop =
    shutdown(signal = None).void.memoize

  def stop = memoizedStop

  def shutdown(
    signal: Option[ProcessSignal] = None,
    restart: Boolean = false,
    dontWaitForDirector: Boolean = false)
  : Task[ProgramTermination] =
    commandExecutor.shutdown(
      ShutDown(signal, restart = restart, dontWaitForDirector = dontWaitForDirector))

  @TestOnly
  def subagentRunId: SubagentRunId =
    commandExecutor.subagentRunId

  @TestOnly
  def testEventBus = commandExecutor.testEventBus
}

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
  : Resource[Task, BareSubagent] =
    Resource.suspend(Task {
      import conf.config
      implicit val s: Scheduler = js7Scheduler

      val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
        .orThrow
      val inMemoryJournalSize = config.getInt("js7.journal.event-buffer-size")

      (for {
        iox <- IOExecutor.resource[Task](config, name = conf.name + "-I/O")
        // For BlockingInternalJob (thread-blocking Java jobs)
        blockingInternalJobScheduler <-
          schedulerServiceToResource(Task(
            newUnlimitedScheduler("JS7 blocking job", conf.config)
          )).map(CorrelId.enableScheduler(_))
        clock <- AlarmClock.resource[Task](Some(alarmClockCheckingInterval))
        journal = new InMemoryJournal(SubagentState.empty,
          size = inMemoryJournalSize,
          waitingFor = "JS7 Agent Director")
        commandExecutor <- conf
          .toJobLauncherConf(iox, blockingInternalJobScheduler, clock)
          .flatMap(SubagentCommandExecutor.checkedResource(journal, conf, _)(iox))
          .orThrow
        actorSystem <- Akkas.actorSystemResource(conf.name, config)
        sessionRegister = SessionRegister.start[SimpleSession](
          actorSystem, SimpleSession(_), config)
        _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)

        webServer <- SubagentWebServer.resource(
          journal, commandExecutor, sessionRegister, restartAsDirector, conf)(actorSystem)
        _ <- provideUriFile(conf, webServer.localHttpUri)
        subagent <- Service.resource(Task(
          new BareSubagent(commandExecutor, journal)))
      } yield {
        logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
        subagent
      }).executeOn(js7Scheduler)
    })

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
