package js7.subagent

import akka.actor.ActorSystem
import cats.Applicative
import cats.effect.{Resource, Sync}
import js7.base.auth.SimpleUser
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.journal.EventIdGenerator
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

final class StandaloneSubagent(
  protected val subagentConf: SubagentConf,
  protected val jobLauncherConf: JobLauncherConf,
  val commandExecutor: SubagentCommandExecutor,
  val webServer: AkkaWebServer with AkkaWebServer.HasUri,
  val journal: InMemoryJournal[SubagentState],
  implicit val scheduler: Scheduler,
  implicit val actorSystem: ActorSystem)
{
  private val release = SetOnce[Task[Unit]]
  private val stoppedOnce = SetOnce[ProgramTermination]

  /** Completes when Subagent has stopped. */
  val untilStopped: Task[ProgramTermination] =
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

object StandaloneSubagent
{
  private val logger = Logger[this.type]

  // Startable without Scheduler
  def blockingRun(conf: SubagentConf, orCommon: Option[Scheduler] = None)
  : ProgramTermination =
    ThreadPools
      .standardSchedulerResource[Coeval](conf.name, conf.config, orCommon)
      .use(implicit scheduler => Coeval {
        resource(conf)
          .use(_.untilStopped)
          .runSyncUnsafe()
      })
      .apply()

  def resource(conf: SubagentConf)(implicit scheduler: Scheduler)
  : Resource[Task, StandaloneSubagent] =
    rawResource(conf)

  private def rawResource(conf: SubagentConf)(implicit scheduler: Scheduler)
  : Resource[Task, StandaloneSubagent] =
    StandaloneSubagent
      .unstartedResource(conf)
      .flatMap(subagent => Resource.make(
        acquire = Task.pure(subagent)
          //.start
          /*.executeOn(scheduler)*/)(
        release = _
          .shutdown(Some(SIGKILL))
          .void
          .executeOn(scheduler)))

  private def unstartedResource(conf: SubagentConf)(implicit scheduler: Scheduler)
  : Resource[Task, StandaloneSubagent] =
    Resource.suspend(
      Task {
        import conf.config
        val clock = AlarmClock(Some(config
          .getDuration("js7.time.clock-setting-check-interval")
          .toFiniteDuration))
        for {
          actorSystem <- Akkas.actorSystemResource(conf.name, config, scheduler)
          iox <- IOExecutor.resource(config, name = conf.name + " I/O")
          eventIdGenerator = new EventIdGenerator
          journal = new InMemoryJournal(SubagentState.empty, eventIdGenerator)

          // For BlockingInternalJob (thread-blocking Java jobs)
          blockingJobScheduler <- Resource.make(
            acquire = Task(newUnlimitedScheduler("JS7 blocking job")))(
            release = o => Task(o.shutdown()))
          jobLauncherConf = conf.toJobLauncherConf(iox, blockingJobScheduler, clock).orThrow
          commandExecutor = new SubagentCommandExecutor(journal, conf, jobLauncherConf)
          sessionRegister = SessionRegister.start[SimpleSession](
            actorSystem, SimpleSession(_), conf.config)
          webServer <- SubagentWebServer
            .resource(
              journal, commandExecutor, sessionRegister, conf)(actorSystem)
          uriFile = conf.workDirectory / "http-uri"
          _ <- provideFile(uriFile).evalTap(_ => Task {
            for (uri <- webServer.localHttpUri) {
              uriFile := s"$uri/subagent"
            }
          })
          _ <- sessionRegister
            .provideSessionTokenFile(SimpleUser.System, conf.workDirectory / "session-token")
        } yield {
          logger.info("Subagent is ready" + "\n" + "─" * 80)
          new StandaloneSubagent(conf, jobLauncherConf, commandExecutor, webServer,
            journal, scheduler, actorSystem)
        }
      }.executeOn(scheduler))

  def threadPoolResource[F[_]](conf: SubagentConf, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F], FA: Applicative[F])
  : Resource[F, Scheduler] =
    ThreadPools
      .standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)
}
