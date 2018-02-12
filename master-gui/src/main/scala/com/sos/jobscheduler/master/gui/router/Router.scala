package com.sos.jobscheduler.master.gui.router

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.gui.components.order.OrderComponent
import com.sos.jobscheduler.master.gui.components.state.OrdersState.FetchedContent
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.components.workflowlist.WorkflowListComponent
import com.sos.jobscheduler.master.gui.components.workfloworders.WorkflowOrdersComponent
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.window
import scala.scalajs.js.URIUtils.{decodeURIComponent, encodeURI}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object Router {
  private val DefaultTitle = "JobScheduler Master"
  private val WorkflowPrefix = "#workflow/"
  private val OrderPrefix = "#order/"

  def route(stateSnapshot: StateSnapshot[GuiState]): TagMod = {
    val state = stateSnapshot.value
    if (state.ordersState.content == OrdersState.FetchingContent)
      <.div(<.i("Waiting for JobScheduler response..."))
    else
      selectPage(stateSnapshot)
  }

  private def selectPage(stateSnapshot: StateSnapshot[GuiState]): TagMod = {
    val state = stateSnapshot.value
    try state.uriHash match {
      case "" | "#" ⇒
        window.document.title = DefaultTitle
        WorkflowListComponent(state.workflowListProps)

      case h if h startsWith WorkflowPrefix ⇒
        val path = decodeURIComponent(h.stripPrefix(WorkflowPrefix))
        if (path startsWith "/") {
          window.document.location.replace(hash(WorkflowPath(path)))
          "..."
        } else {
          val workflowPath = WorkflowPath(s"/$path")
          window.document.title = workflowPath.pretty
          WorkflowOrdersComponent(
            state.pathToWorkflow(workflowPath),
            StateSnapshot(state.ordersState)(s ⇒ stateSnapshot.modState(_.copy(ordersState = s))))
        }

      case h if h startsWith OrderPrefix ⇒
        val orderId = OrderId(decodeURIComponent(h.stripPrefix(OrderPrefix)))
        window.document.title = orderId.pretty
        state.ordersState.content match {
          case content: FetchedContent ⇒
            val orderEntry = content.idToEntry.getOrElse(orderId, sys.error(s"Unknown $orderId"))
            val workflow = state.pathToWorkflow(orderEntry.order.workflowPath)
            OrderComponent(orderEntry, content.idToEntry(_).order, workflow, isOwnPage = true)

          case _ ⇒
            <.div(^.cls := "error", "No data")
        }

      case _ ⇒
        window.document.title = DefaultTitle
        <.div(^.cls := "error", s"Unrecognized URI ${window.document.location}")
    } catch {
      case NonFatal(t) ⇒
        window.document.title = DefaultTitle
        <.div(^.cls := "error", t.toStringWithCauses)
    }
  }

  def hash(workflowPath: WorkflowPath) =
    WorkflowPrefix + encodeHashPath(workflowPath)

  def hash(orderId: OrderId) =
    try OrderPrefix + encodeURI(orderId.string)
    catch { case t: Throwable ⇒
      window.console.error(s"$orderId: $t")
      ""
    }

  private def encodeHashPath(path: TypedPath): String =
    try encodeURI(path.withoutStartingSlash)
    catch { case t: Throwable ⇒
      window.console.error(s"$path: $t")
      ""
    }
}
