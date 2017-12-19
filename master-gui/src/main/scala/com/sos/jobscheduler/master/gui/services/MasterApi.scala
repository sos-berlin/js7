package com.sos.jobscheduler.master.gui.services

import com.sos.jobscheduler.master.client.{HttpMasterApi, JsHttpClient}
import com.sos.jobscheduler.master.gui.services.JsBridge.jobschedulerBuildId
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
object MasterApi extends HttpMasterApi("/",
  new JsHttpClient(jobschedulerBuildId.ensuring(_.nonEmpty, "GUI error: Variable jobschedulerBuildId is empty")))
