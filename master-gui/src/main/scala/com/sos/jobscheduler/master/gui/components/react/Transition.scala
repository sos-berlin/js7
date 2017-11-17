package com.sos.jobscheduler.master.gui.components.react

import japgolly.scalajs.react
import japgolly.scalajs.react.Children
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.VdomNode
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

object Transition {
  // https://github.com/reactjs/react-transition-group/blob/master/Migration.md

  @JSImport("react-transition-group/Transition", JSImport.Namespace, "React.addons.TransitionGroup")
  @js.native
  object TransitionGroup extends js.Object

  @JSImport("react-transition-group/Transition", JSImport.Namespace, "React.addons.CSSTransition")
  @js.native
  object CSSTransitition extends js.Object

  @js.native
  trait Props extends js.Object {
    var component: String = js.native
    var classNames   : js.UndefOr[String | ReactCSSTransitionGroupNames]
    var appear       : js.UndefOr[Boolean]
    var enter        : js.UndefOr[Boolean]
    var leave        : js.UndefOr[Boolean]
    var appearTimeout: js.UndefOr[JsNumber]
    var enterTimeout : js.UndefOr[JsNumber]
    var transitionLeaveTimeout : js.UndefOr[JsNumber]
  }

  @js.native
  trait ReactCSSTransitionGroupNames extends js.Object {
    var appear      : js.UndefOr[String]
    var  enter      : js.UndefOr[String]
    var  leave      : js.UndefOr[String]
    var appearActive: js.UndefOr[String]
    var  enterActive: js.UndefOr[String]
    var  leaveActive: js.UndefOr[String]
  }

  def apply(component: String,
    classNames: String,
    appear: js.UndefOr[Boolean] = js.undefined,
    appearTimeout: js.UndefOr[JsNumber] = js.undefined,
    transitionEnter: js.UndefOr[Boolean] = js.undefined,
    enterTimeout: js.UndefOr[JsNumber] = js.undefined)
    (children: VdomNode*)
  = {
    val p = (new js.Object).asInstanceOf[Props]
    p.component = component
    p.classNames = classNames
    p.appear = appear
    p.appearTimeout = appearTimeout
    p.enter = transitionEnter
    p.enterTimeout = enterTimeout
    this.component(p)(children: _*)
  }

  val component = react.JsComponent[Props, Children.Varargs, Null](TransitionGroup)
}
