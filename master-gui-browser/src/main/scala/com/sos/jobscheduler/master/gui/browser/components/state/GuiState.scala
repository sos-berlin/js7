package com.sos.jobscheduler.master.gui.browser.components.state

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.master.gui.browser.components.workflowlist.WorkflowListComponent
import org.scalajs.dom.window

/**
  * @author Joacim Zschimmer
  */
final case class GuiState(
  ordersState: OrdersState,
  idToWorkflow: Map[WorkflowId, PreparedWorkflow],
  appState: AppState,
  isConnected: Boolean,
  uriHash: String = window.document.location.hash)
  //hashToPosition: Map[String, Position] = Map.empty)
{
  lazy val workflowListProps = WorkflowListComponent.Props(
    (for (prepared ← idToWorkflow.values) yield
      WorkflowListComponent.Props.Entry(prepared.workflow, ordersState.orderCountByWorkflow(prepared.id))
    ).toImmutableSeq)

  def updateUriHash = copy(
    uriHash = window.document.location.hash)

  //def memoizePositionForUri(uri: String) = copy(
  //  hashToPosition = hashToPosition + (hashOfUri(uri) → GuiState.Position(dom.window.screenX, dom.window.screenY)))

  //def restoreCurrentScreenPositionIfHashChanged() = Callback {
  //  if (dom.window.location.hash != uriHash) {
  //    hashToPosition.get(dom.window.location.hash) match {
  //      case Some(Position(x, y)) ⇒ dom.window.scrollTo(x, y)
  //      case None ⇒
  //    }
  //  }
  //}
}

object GuiState {
  val Initial = GuiState(OrdersState.Empty, Map.empty, AppState.RequestingEvents, isConnected = false)

  def hashOfUri(uri: String) =
    uri indexOf '#' match {
      case -1 ⇒ ""
      case i ⇒ uri.substring(i)
    }

  final case class Position(x: Int, y: Int)
}
