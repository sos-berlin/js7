package js7.master.web.master.api

import akka.http.scaladsl.server.Directives.get
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.PathDirectives.pathSingleSlash
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers.monixObservableToMarshallable
import js7.common.http.CirceJsonSupport.jsonMarshaller
import js7.core.web.StampedStreamingSupport.stampedCirceStreamingSupport
import js7.master.data.MasterSnapshots.SnapshotJsonCodec
import js7.master.data.MasterState
import js7.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler

trait SnapshotRoute extends MasterRouteProvider
{
  protected def masterState: Task[Checked[MasterState]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val snapshotRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathSingleSlash {
          completeTask(
            for (checkedState <- masterState) yield
              for (state <- checkedState) yield {
                implicit val x = stampedCirceStreamingSupport(eventId = state.eventId)
                implicit val y = SnapshotJsonCodec
                monixObservableToMarshallable(state.toSnapshotObservable)
              })
        }
      }
    }
}
