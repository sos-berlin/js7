package js7.common.system.startup

import cats.effect.{Resource, SyncIO}
import com.typesafe.config.Config
import izumi.reflect.Tag
import js7.base.service.MainService
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.utils.AllocatedForJvm.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.SyncResource.syntax.*
import js7.common.system.ThreadPools
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.*

// Needs access to js7-common/ThreadPools
object MainServices
{
  /** Run a MainService with its own ThreadPool. */
  def blockingRun[S <: MainService: Tag](
    threadPoolName: String,
    config: Config,
    timeout: Duration = Duration.Inf)(
    resource: Scheduler => Resource[Task, S],
    use: (S, Scheduler) => ProgramTermination = (service: S, scheduler: Scheduler) => {
      implicit val s = scheduler
      service.untilTerminated.await(timeout)
    })
  : ProgramTermination =
    ThreadPools
      .standardSchedulerResource[SyncIO](threadPoolName, config)
      .useSync { implicit scheduler =>
        resource(scheduler)
          .toAllocated
          .await(timeout)
          .useSync(timeout)(use(_, scheduler))
      }
}
