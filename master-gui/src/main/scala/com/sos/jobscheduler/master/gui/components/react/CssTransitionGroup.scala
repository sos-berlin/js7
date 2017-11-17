package com.sos.jobscheduler.master.gui.components.react

import japgolly.scalajs.react
import japgolly.scalajs.react.Children
import japgolly.scalajs.react.raw.{JsNumber, ReactCSSTransitionGroupNames}
import japgolly.scalajs.react.vdom.VdomNode
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

object CssTransitionGroup {
  @JSImport("react-addons-css-transition-group", JSImport.Namespace, "React.addons.CSSTransitionGroup")
  @js.native
  object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var component: String = js.native
    var transitionName         : js.UndefOr[String | ReactCSSTransitionGroupNames]
    var transitionAppear       : js.UndefOr[Boolean]
    var transitionEnter        : js.UndefOr[Boolean]
    var transitionLeave        : js.UndefOr[Boolean]
    var transitionAppearTimeout: js.UndefOr[JsNumber]
    var transitionEnterTimeout : js.UndefOr[JsNumber]
    var transitionLeaveTimeout : js.UndefOr[JsNumber]
  }

  def apply(component: String,
    transitionName: String,
    transitionAppear: js.UndefOr[Boolean] = js.undefined,
    transitionAppearTimeout: js.UndefOr[JsNumber] = js.undefined,
    transitionEnter: js.UndefOr[Boolean] = js.undefined,
    transitionEnterTimeout: js.UndefOr[JsNumber] = js.undefined)
    (children: VdomNode*)
  = {
    val p = (new js.Object).asInstanceOf[Props]
    p.component = component
    p.transitionName = transitionName
    p.transitionAppear = transitionAppear
    p.transitionAppearTimeout = transitionAppearTimeout
    p.transitionEnter = transitionEnter
    p.transitionEnterTimeout = transitionEnterTimeout
    this.component(p)(children: _*)
  }

  val component = react.JsComponent[Props, Children.Varargs, Null](RawComponent)
}
