package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives.get
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.PathDirectives.pathSingleSlash
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers._
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, jsonSeqMarshaller}
import js7.common.scalautil.Logger
import js7.controller.data.ControllerSnapshots.SnapshotJsonCodec
import js7.controller.data.ControllerState
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.SnapshotRoute._
import monix.eval.Task
import monix.execution.Scheduler

trait SnapshotRoute extends ControllerRouteProvider
{
  protected def controllerState: Task[Checked[ControllerState]]

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  final val snapshotRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathSingleSlash {
          completeTask(
            for (checkedState <- controllerState) yield
              for (state <- checkedState) yield {
                implicit val x = NdJsonStreamingSupport
                implicit val y = SnapshotJsonCodec
                implicit val z = jsonSeqMarshaller[Any]
                monixObservableToMarshallable(
                  state.toSnapshotObservable
                    .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
                      Task { logger.debug("whenShuttingDown completed") }))
              })
        }
      }
    }
}

object SnapshotRoute
{
  private val logger = Logger(getClass)
}
