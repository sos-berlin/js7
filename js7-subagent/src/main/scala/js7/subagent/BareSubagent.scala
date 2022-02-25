package js7.subagent

import cats.Applicative
import cats.effect.{Resource, Sync}
import js7.base.auth.SimpleUser
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.{newUnlimitedScheduler, schedulerServiceToResource}
import js7.journal.watch.InMemoryJournal
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

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
    restart: Boolean = false)
  : Task[ProgramTermination] =
    commandExecutor.shutdown(signal, restart)
}

object BareSubagent
{
  private val logger = Logger[this.type]

  // Startable without Scheduler
  def blockingRun(conf: SubagentConf, orCommon: Option[Scheduler] = None)
  : ProgramTermination =
    ThreadPools
      .standardSchedulerResource[Coeval](conf.name, conf.config, orCommon)
      .use(implicit scheduler => Coeval {
        resource(conf, scheduler)
          .use(_.untilStopped)
          .runSyncUnsafe()
      })
      .apply()

  def resource(conf: SubagentConf, js7Scheduler: Scheduler): Resource[Task, BareSubagent] = {
    import conf.config
    implicit val s = js7Scheduler

    (for {
      iox <- IOExecutor.resource(config, name = conf.name + " I/O")
      // For BlockingInternalJob (thread-blocking Java jobs)
      blockingInternalJobScheduler <- schedulerServiceToResource(Task(
        newUnlimitedScheduler("JS7 blocking job")))
      clock <- AlarmClock.resource(Some(config
        .getDuration("js7.time.clock-setting-check-interval")
        .toFiniteDuration))
      journal = new InMemoryJournal(SubagentState.empty)
      commandExecutor = new SubagentCommandExecutor(journal, conf,
        conf.toJobLauncherConf(iox, blockingInternalJobScheduler, clock).orThrow)

      actorSystem <- Akkas.actorSystemResource(conf.name, config, js7Scheduler)
      sessionRegister = SessionRegister.start[SimpleSession](
        actorSystem, SimpleSession(_), conf.config)
      _ <- sessionRegister
        .provideSessionTokenFile(SimpleUser.System, conf.workDirectory / "session-token")

      webServer <- SubagentWebServer.resource(
        journal, commandExecutor, sessionRegister, conf)(actorSystem)
      uriFile = conf.workDirectory / "http-uri"
      _ <- provideFile(uriFile).evalTap(_ => Task {
        for (uri <- webServer.localHttpUri) {
          uriFile := s"$uri/subagent"
        }
      })

      subagent <- Resource.make(
        acquire = Task.pure(
          new BareSubagent(commandExecutor, journal)))(
        release = _
          // Normally, SubagentCommand.ShutDown should have been processed to arrive here
          .shutdown(Some(SIGKILL)).void)
    } yield {
      logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
      subagent
    }).executeOn(js7Scheduler)
  }

  def threadPoolResource[F[_]](conf: SubagentConf, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F], FA: Applicative[F])
  : Resource[F, Scheduler] =
    ThreadPools
      .standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)
}
