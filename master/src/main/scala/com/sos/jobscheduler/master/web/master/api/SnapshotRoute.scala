package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives.get
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.PathDirectives.pathSingleSlash
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers.monixObservableToMarshallable
import com.sos.jobscheduler.common.http.CirceJsonSupport.jsonMarshaller
import com.sos.jobscheduler.core.web.StampedStreamingSupport.stampedCirceStreamingSupport
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
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
