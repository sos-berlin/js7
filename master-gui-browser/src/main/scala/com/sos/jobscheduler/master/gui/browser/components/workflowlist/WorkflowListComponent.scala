package com.sos.jobscheduler.master.gui.browser.components.workflowlist

import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.gui.browser.router.Router
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, ScalaComponent}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object WorkflowListComponent {

  def apply(props: Props) = scalaComponent(props)

  private val scalaComponent = ScalaComponent.builder[Props]("WorkflowList")
    .renderBackend[Backend]
    .build

  final class Backend(scope: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement =
      <.div(
        <.div(^.cls := "sheet-headline")(
          s"${props.entries.size} Workflows"),
        props.entries.sortBy(_.id).toVdomArray(e ⇒ EntryComponent(e)))
  }

  final case class Props(entries: Seq[Props.Entry])
  object Props {
    final case class Entry(workflow: Workflow, orderCount: Option[Int]) {
      def id = workflow.id
    }
    object Entry {
      implicit val reuse: Reusability[Entry] = Reusability.byRef[Entry]
    }
    implicit val reuse: Reusability[Props] = Reusability.byRef[Props]
  }

  private val EntryComponent = ScalaComponent.builder[Props.Entry]("WorkflowList.Workflow")
    .render_P { entry ⇒
      <.div(
        <.a(^.cls := "hidden-link", ^.href := Router.hash(entry.workflow.id))(
          <.b(entry.id.pretty)),
        entry.orderCount.whenDefined(n ⇒ s" · $n orders"))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
