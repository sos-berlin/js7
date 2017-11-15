package com.sos.jobscheduler.master.gui.components

import com.sos.jobscheduler.master.gui.services.MasterApi
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}

/**
  * @author Joacim Zschimmer
  */
object SideBarComponent {

  private val RabbitPictureUri = "api/frontend/common/images/job_scheduler_rabbit_circle_60x60.gif"

  type State = Unit

  def apply() = reactComponent()

  private val reactComponent = ScalaComponent.builder[Unit]("SideBar")
    .initialState[State](None)
    .renderBackend[PageHeaderBackend]
    .build

  final class PageHeaderBackend(scope: BackendScope[Unit, State]) {
    def render(state: State) =
      <.ul(^.id := "nav-mobile", ^.cls := "side-nav fixed")(
        <.li(^.cls := "logo")(
          <.a(^.id := "logo-container", ^.href := ".", ^.cls := "brand-logo")(
            <.img(^.width := "40", ^.height := "40", ^.alt := "Rabbit", ^.src := RabbitPictureUri))),
        //<.li(
        //  <.a(^.href := "#orders")(
        //    "Orders")),
        <.li(
          <.button(^.cls := "waves-effect waves-light btn deep-orange", ^.onClick --> terminate)(
            "Shutdown")))
  }

  private def terminate = Callback {
    MasterApi.executeCommand("""{"TYPE": "Terminate"}""")
  }
}
