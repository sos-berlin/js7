package com.sos.jobscheduler.master.gui.browser.router

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.filebased.{FileBasedId, TypedPath, VersionId}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import com.sos.jobscheduler.master.gui.browser.components.order.OrderComponent
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.FetchedContent
import com.sos.jobscheduler.master.gui.browser.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.browser.components.workflowlist.WorkflowListComponent
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.WorkflowOrdersComponent
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
    if (state.ordersState.content == OrdersState.Starting)
      <.div(<.i("Starting..."))
    else
    if (state.ordersState.content == OrdersState.FetchingContent)
      <.div(<.i("Waiting for JobScheduler response..."))
    else
      selectPage(stateSnapshot)
  }

  private def selectPage(stateSnapshot: StateSnapshot[GuiState]): TagMod = {
    val guiState = stateSnapshot.value
    try guiState.uriHash match {
      case "" | "#" ⇒
        window.document.title = DefaultTitle
        WorkflowListComponent(guiState.workflowListProps)

      case h if h startsWith WorkflowPrefix ⇒
        val originalPathAndVersion = decodeURIComponent(h.stripPrefix(WorkflowPrefix))
        val pathAndVersion = "/" + originalPathAndVersion.stripPrefix("/")
        val checkedPathAndVersion: Checked[(WorkflowPath, Option[VersionId])] = {
          val q = "?version="
          pathAndVersion indexOf q match {
            case -1 ⇒ for (path ← WorkflowPath.checked(pathAndVersion)) yield path → None
            case i ⇒
              for {
                path ← WorkflowPath.checked(pathAndVersion take i)
                version ← VersionId.checked(pathAndVersion drop i + q.length)
              } yield path → Some(version)
          }
        }
        checkedPathAndVersion match {
          case Invalid(problem) ⇒ <.div(^.cls := "error", s"Unrecognized URI ${window.document.location}: $problem")
          case Valid((path, None)) ⇒
            if (originalPathAndVersion startsWith "/") {
              window.document.location.replace(hash(path))
              "..."
            } else
              <.div(^.cls := "error", "Missing VersionId")
          case Valid((path, Some(version))) ⇒
            val id = path % version
            if (originalPathAndVersion startsWith "/") {
              window.document.location.replace(hash(id))
              "..."
            } else {
              window.document.title = id.pretty
              WorkflowOrdersComponent(
                guiState.idToWorkflow(id),
                StateSnapshot(guiState.ordersState)((orderStateOption, callback) ⇒
                  stateSnapshot.modStateOption(
                    guiState ⇒ orderStateOption map (o ⇒ guiState.copy(ordersState = o)),
                    callback)))
          }
        }

      case h if h startsWith OrderPrefix ⇒
        val orderId = OrderId(decodeURIComponent(h.stripPrefix(OrderPrefix)))
        window.document.title = orderId.pretty
        guiState.ordersState.content match {
          case content: FetchedContent ⇒
            val orderEntry = content.idToEntry.getOrElse(orderId, sys.error(s"Unknown $orderId"))
            val workflow = guiState.idToWorkflow(orderEntry.order.workflowId)
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

  def hash(workflowId: WorkflowId) =
    WorkflowPrefix + encodeHashVersionedPath(workflowId)

  def hash(workflowPath: WorkflowPath) =
    WorkflowPrefix + encodeHashPath(workflowPath)

  def hash(orderId: OrderId) =
    try OrderPrefix + encodeURI(orderId.string)
    catch { case t: Throwable ⇒
      window.console.error(s"$orderId: $t")
      ""
    }

  private def encodeHashVersionedPath[P <: TypedPath](id: FileBasedId[P]): String =
    try encodeURI(id.path.withoutStartingSlash) + "?version=" + encodeURI(id.versionId.string)
    catch { case t: Throwable ⇒
      window.console.error(s"$id: $t")
      ""
    }

  private def encodeHashPath(path: TypedPath): String =
    try encodeURI(path.withoutStartingSlash)
    catch { case t: Throwable ⇒
      window.console.error(s"$path: $t")
      ""
    }
}
