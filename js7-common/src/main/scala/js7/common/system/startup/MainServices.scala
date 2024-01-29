//package js7.common.system.startup
//
//import cats.effect.{IO, Resource, SyncIO}
//import com.typesafe.config.Config
//import izumi.reflect.Tag
//import js7.base.service.MainService
//import js7.base.utils.AllocatedForJvm.*
//import js7.base.utils.CatsUtils.syntax.RichResource
//import js7.base.utils.ProgramTermination
//import js7.base.utils.SyncResource.syntax.*
//import js7.common.system.ThreadPools
//import scala.concurrent.duration.*
//import cats.effect.unsafe.IORuntime
//import js7.base.thread.CatsBlocking.syntax.await
//
//// Needs access to js7-common/ThreadPools
//object MainServices:
//
//  /** Run a MainService. */
//  @deprecated("Use ServiceMain.blockingRun (?)", "v2.7")
//  def blockingRun[S <: MainService](
//    name: String,
//    timeout: Duration = Duration.Inf)
//    (using rt: IORuntime, S: Tag[S])
//    (resource: Resource[IO, S],
//    use: S => ProgramTermination = (_: S).untilTerminated)
//  : ProgramTermination =
//    resource
//      .toAllocated
//      .await(timeout)
//      .useSync(timeout)(use)
