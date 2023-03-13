package js7.common.system.startup

import cats.effect.{Resource, SyncIO}
import com.typesafe.config.Config
import js7.base.service.MainService
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ProgramTermination
import js7.base.utils.SyncResource.syntax.*
import js7.common.system.ThreadPools
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.*

// Needs access to js7-common/ThreadPools
object MainServices
{
  /** Adds an own ThreadPool. */
  def blockingRun[S <: MainService](
    name: String,
    config: Config,
    timeout: Duration = Duration.Inf)(
    resource: Scheduler => Resource[Task, S],
    use: (S, Scheduler) => ProgramTermination = (service: S, scheduler: Scheduler) => {
      implicit val s = scheduler
      service.untilTerminated.await(timeout)
    })
  : ProgramTermination =
    ThreadPools
      .standardSchedulerResource[SyncIO](name, config)
      .useSync(implicit scheduler =>
        resource(scheduler)
          .use(service => Task(
            use(service, scheduler)))
          .await(timeout + 1.s/*allow `use` to fail first*/))
}
