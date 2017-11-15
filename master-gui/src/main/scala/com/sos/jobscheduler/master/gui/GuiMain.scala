package com.sos.jobscheduler.master.gui

import com.sos.jobscheduler.master.gui.components.gui.GuiComponent
import org.scalajs.dom

/**
  * @author Joacim Zschimmer
  */
object GuiMain {

  def main(args: Array[String]): Unit =
    GuiComponent().renderIntoDOM(dom.document.getElementById("GUI"))
}
