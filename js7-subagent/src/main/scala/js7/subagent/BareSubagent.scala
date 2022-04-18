package js7.subagent

import cats.Applicative
import cats.effect.{Resource, Sync}
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.RichConfig
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
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
import monix.catnap.MVar
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly

final class BareSubagent(
  private val commandExecutor: SubagentCommandExecutor,
  val journal: InMemoryJournal[SubagentState])
{
  private val release = SetOnce[Task[Unit]]
  private val stoppedOnce = SetOnce[ProgramTermination]

  /** Completes when Subagent has stopped. */
  def untilStopped: Task[ProgramTermination] =
    commandExecutor.untilStopped

  protected def onStopped(termination: ProgramTermination) =
    Task.defer {
      release.orThrow.map { _ =>
        // TODO Beware race condition?
        stoppedOnce.trySet(termination)
      }
    }

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
}

object BareSubagent
{
  private val logger = Logger[this.type]

  type StartAsAgentDirector = StartAsAgentDirector.type
  object StartAsAgentDirector

  // Startable without Scheduler
  def blockingRun(conf: SubagentConf, orCommonScheduler: Option[Scheduler] = None)
  : Either[StartAsAgentDirector, ProgramTermination] = {
    ThreadPools
      .standardSchedulerResource[Coeval](conf.name, conf.config, orCommonScheduler)
      .use(implicit scheduler => Coeval {
        val restartAsDirectorVar = MVar.empty[Task, Unit]().memoize
        val restartAsDirector = restartAsDirectorVar.flatMap(_.tryPut(())).void
        resource(conf, scheduler, restartAsDirector)
          .use(bareSubagent => Task
            .race(
              restartAsDirectorVar.flatMap(_.take),
              bareSubagent.untilStopped)
            .flatMap {
              case Left(()) =>
                bareSubagent.shutdown(dontWaitForDirector = true)
                  .as(Left(StartAsAgentDirector))
              case Right(o) => Task.right(o)
            })
          .runSyncUnsafe()
      })
      .apply()
  }

  def resource(conf: SubagentConf, js7Scheduler: Scheduler, restartAsDirector: Task[Unit] = Task.unit)
  : Resource[Task, BareSubagent] =
    Resource.suspend(Task {
      import conf.config
      implicit val s = js7Scheduler

      val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
        .orThrow
      val inMemoryJournalSize = config.getInt("js7.journal.event-buffer-size")

      (for {
        iox <- IOExecutor.resource(config, name = conf.name + " I/O")
        // For BlockingInternalJob (thread-blocking Java jobs)
        blockingInternalJobScheduler <- schedulerServiceToResource(Task(
          newUnlimitedScheduler("JS7 blocking job")))
        clock <- AlarmClock.resource(Some(alarmClockCheckingInterval))
        journal = new InMemoryJournal(SubagentState.empty,
          size = inMemoryJournalSize,
          waitingFor = "JS7 Agent Director")
        commandExecutor = new SubagentCommandExecutor(journal, conf,
          conf.toJobLauncherConf(iox, blockingInternalJobScheduler, clock).orThrow)

        actorSystem <- Akkas.actorSystemResource(conf.name, config, js7Scheduler)
        sessionRegister = SessionRegister.start[SimpleSession](
          actorSystem, SimpleSession(_), config)
        _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)

        webServer <- SubagentWebServer.resource(
          journal, commandExecutor, sessionRegister, restartAsDirector, conf)(actorSystem)
        _ <- provideUriFile(conf, webServer.localHttpUri)
        subagent <- Resource.make(
          acquire = Task(new BareSubagent(commandExecutor, journal)))(
          release = _.shutdown(signal = None).void)
      } yield {
        logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
        subagent
      }).executeOn(js7Scheduler)
    })

  private def provideUriFile(conf: SubagentConf, uri: Checked[Uri]) = {
    val uriFile = conf.workDirectory / "http-uri"
    provideFile(uriFile)
      .evalTap(_ => Task {
        for (uri <- uri) uriFile := s"$uri/subagent"
      })
  }

  def threadPoolResource[F[_]](conf: SubagentConf, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F], FA: Applicative[F])
  : Resource[F, Scheduler] =
    ThreadPools
      .standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)
}
