package com.sos.jobscheduler.master.gui.services

import com.sos.jobscheduler.master.client.{JsHttpClient, MasterApi ⇒ Api}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
object MasterApi extends Api(JsHttpClient, uri = "")
