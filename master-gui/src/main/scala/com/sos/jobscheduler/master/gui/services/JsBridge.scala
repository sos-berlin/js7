package com.sos.jobscheduler.master.gui.services

import org.scalajs.jquery.JQueryStatic
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

/**
  * @author Joacim Zschimmer
  */
@js.native
@JSGlobalScope
object JsBridge extends js.Object {

  var jobschedulerBuildId: String = js.native
  var jobschedulerBuildVersion: String = js.native
  val jQuery: JQueryStatic = js.native
}
