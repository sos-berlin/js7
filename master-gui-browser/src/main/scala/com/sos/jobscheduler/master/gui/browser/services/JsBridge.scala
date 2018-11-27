package com.sos.jobscheduler.master.gui.browser.services

import org.scalajs.jquery.JQueryStatic
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

/**
  * @author Joacim Zschimmer
  */
@js.native
@JSGlobalScope
object JsBridge extends js.Object {

  val guiConfig: GuiConfig = js.native
  val jQuery: JQueryStatic = js.native
  val toastr: Toastr = js.native
}
