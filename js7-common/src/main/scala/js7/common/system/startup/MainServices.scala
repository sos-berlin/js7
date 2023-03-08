package js7.common.system.startup

import cats.effect.{Resource, SyncIO}
import com.typesafe.config.Config
import js7.base.service.MainService
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.utils.ProgramTermination
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.common.system.ThreadPools
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Duration

// Needs access to js7-common/ThreadPools
object MainServices
{
  /** Adds an own ThreadPool. */
  def blockingRun[S <: MainService](
    name: String,
    config: Config,
    timeout: Duration = Duration.Inf)(
    resource: Scheduler => Resource[Task, S],
    use: S => Task[ProgramTermination] = (_: S).untilTerminated)
  : ProgramTermination =
    ThreadPools
      .standardSchedulerResource[SyncIO](name, config)
      .useSync(implicit scheduler =>
        resource(scheduler).use(use).await(timeout))
}
