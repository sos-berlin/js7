package com.sos.jobscheduler.master.gui.browser

import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom.window

/**
  * @author Joacim Zschimmer
  */
object GuiMain {

  def main(args: Array[String]): Unit = {
    val baseUrl = BaseUrl.fromWindowOrigin / "master"
    GuiComponent(baseUrl = baseUrl).renderIntoDOM(window.document.getElementById("GUI"))
  }
}
