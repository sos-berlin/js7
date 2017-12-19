package com.sos.jobscheduler.master.gui

import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom

/**
  * @author Joacim Zschimmer
  */
object GuiMain {

  def main(args: Array[String]): Unit = {
    val baseUrl = BaseUrl.fromWindowOrigin / "master"
    GuiComponent(baseUrl = baseUrl).renderIntoDOM(dom.document.getElementById("GUI"))
  }
}
