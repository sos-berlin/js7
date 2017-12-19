package com.sos.jobscheduler.master.gui.components.state

import org.scalajs.dom

/**
  * @author Joacim Zschimmer
  */
final case class GuiState(
  ordersState: OrdersState,
  isFreezed: Boolean,
  isConnected: Boolean,
  isFetchingState: Boolean,
  uriHash: String = dom.window.document.location.hash)
  //hashToPosition: Map[String, Position] = Map.empty)
{
  def updateUriHash = copy(
    uriHash = dom.window.document.location.hash)

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
  val Initial = GuiState(OrdersState.Empty, isFreezed = false, isConnected = false, isFetchingState = false)

  def hashOfUri(uri: String) =
    uri indexOf '#' match {
      case -1 ⇒ ""
      case i ⇒ uri.substring(i)
    }

  final case class Position(x: Int, y: Int)
}
