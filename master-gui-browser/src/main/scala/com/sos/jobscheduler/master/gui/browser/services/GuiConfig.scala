package com.sos.jobscheduler.master.gui.browser.services

import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
@js.native
trait GuiConfig extends js.Object {
  val buildId: String
  val buildVersion: String
}
