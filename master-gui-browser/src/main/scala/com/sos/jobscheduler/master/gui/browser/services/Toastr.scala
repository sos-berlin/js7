package com.sos.jobscheduler.master.gui.browser.services

import scala.scalajs.js
import scala.scalajs.js.annotation.JSBracketAccess

/**
  * @author Joacim Zschimmer
  */
@js.native
trait Toastr extends js.Object
{
  @JSBracketAccess
  def apply(level: String): js.Function1[String, js.Dynamic] = js.native
}
