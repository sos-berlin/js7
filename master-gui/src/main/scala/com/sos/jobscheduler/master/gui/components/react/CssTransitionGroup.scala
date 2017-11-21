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
  private object RawComponent extends js.Object

  private val Component = react.JsComponent[Props, Children.Varargs, Null](RawComponent)

  // Current version is here: https://github.com/reactjs/react-transition-group
  @deprecated("ReactTransitionGroup and ReactCSSTransitionGroup are both deprecated as of React v15.5.0. The recommendation is to use TransitionGroup and CSSTransitionGroup from 'react-transition-group' instead.", "1.0.1")
  def apply(component: String,
    transitionName: String,
    //appear: js.UndefOr[Boolean] = js.undefined,
    //appearTimeout: js.UndefOr[JsNumber] = js.undefined,
    enter: js.UndefOr[Boolean] = js.undefined,
    enterTimeout: js.UndefOr[JsNumber] = js.undefined)
    (children: VdomNode*)
  = {
    val p = (new js.Object).asInstanceOf[Props]
    p.component = component
    p.transitionName = transitionName
    //p.transitionAppear = appear
    //p.transitionAppearTimeout = appearTimeout
    p.transitionEnter = enter
    p.transitionEnterTimeout = enterTimeout
    Component(p)(children: _*)
  }

  @js.native
  trait Props extends js.Object {
    var component: String = js.native
    var transitionName         : js.UndefOr[String | ReactCSSTransitionGroupNames]
    //var transitionAppear       : js.UndefOr[Boolean]
    var transitionEnter        : js.UndefOr[Boolean]
    //var transitionLeave        : js.UndefOr[Boolean]
    //var transitionAppearTimeout: js.UndefOr[JsNumber]
    var transitionEnterTimeout : js.UndefOr[JsNumber]
    //var transitionLeaveTimeout : js.UndefOr[JsNumber]
  }
}
