package js7.subagent

import akka.actor.ActorSystem
import akka.util.Timeout
import cats.effect.Resource
import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Closer, ProgramTermination, SetOnce}
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.journal.EventIdGenerator
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.StandaloneSubagent.logger
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import monix.eval.Task
import monix.execution.Scheduler

final class StandaloneSubagent(
  protected val subagentConf: SubagentConf,
  protected val jobLauncherConf: JobLauncherConf,
  protected val journal: InMemoryJournal[SubagentState],
  implicit val scheduler: Scheduler,
  implicit val actorSystem: ActorSystem)
extends SubagentCommandExecutor
{
  private val sessionRegister = SessionRegister.start[SimpleSession](
    actorSystem, SimpleSession(_), subagentConf.config)
  private val webServerResource = SubagentWebServer.resource(
    journal, this, sessionRegister, subagentConf)

  private val release = SetOnce[Task[Unit]]
  private val stoppedOnce = SetOnce[ProgramTermination]

  /** Completes when Subagent has stopped. */
  val stopped: Task[ProgramTermination] =
    stoppedOnce.task

  protected def onStopped(termination: ProgramTermination) =
    Task.defer {
      release.orThrow.map { _ =>
        stoppedOnce.trySet(termination)
        termination
      }
    }

  private def start: Task[this.type] =
    webServerResource.allocated
      .map { case (_, releaseWebServer) =>
        logger.info("Subagent is ready" +
          "\n" + "─" * 80)
        //logger.info(s"Subagent '${subagentId.string}' is ready " +
        //  SubagentMain.runningSince.fold("")(o => s" (after ${o.elapsed.pretty})") +
        //  "\n" + "─" * 80)
        release := releaseWebServer
      }
      .as(this)
}

object StandaloneSubagent
{
  private val logger = Logger[this.type]

  // Startable without Scheduler
  def blockingRun(conf: SubagentConf, commonScheduler: Option[Scheduler] = None): ProgramTermination =
    autoClosing(new Closer) { closer =>
      implicit val scheduler = commonScheduler.getOrElse(
        ThreadPools.newStandardScheduler(conf.name, conf.config, closer))
      resource(conf).use(_.stopped)
        .runSyncUnsafe()
    }

  def makeSubagentConf(
    dir: Path,
    webServerPorts: Seq[WebServerPort],
    name: String = "JS7",
    config: Config)
  : SubagentConf =
    SubagentConf(
      configDirectory = dir / "config",
      dataDirectory = dir / "data",
      logDirectory = dir / "logs",
      jobWorkingDirectory = dir,
      webServerPorts = webServerPorts,
      defaultJobSigkillDelay = 15.s,  // TODO
      killScript = None,
      akkaAskTimeout = Timeout(99.s), // TODO
      name = name,
      config.withFallback(SubagentConf.defaultConfig))

  def resource(conf: SubagentConf)(implicit scheduler: Scheduler)
  : Resource[Task, StandaloneSubagent] =
    StandaloneSubagent
      .unstartedResource(conf)
      .flatMap(subagent => Resource.make(
        acquire = subagent
          .start
          .executeOn(scheduler))(
        release = _
          .stop(Some(SIGKILL))
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
        } yield
          new StandaloneSubagent(conf, jobLauncherConf, journal, scheduler, actorSystem)
      }.executeOn(scheduler))

  def threadPoolResource(conf: SubagentConf, commonScheduler: Option[Scheduler] = None)
  : Resource[Task, Scheduler] =
    ThreadPools
      .standardSchedulerResource(conf.name, conf.config, orCommon = commonScheduler)
}
