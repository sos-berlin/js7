package com.sos.jobscheduler.master.gui.browser.services

import com.sos.jobscheduler.common.http.JsHttpClient
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.jobschedulerBuildId

/**
  * @author Joacim Zschimmer
  */
object MasterApi extends HttpMasterApi {
  protected val uri = "/"
  protected val httpClient = new JsHttpClient(jobschedulerBuildId.ensuring(_.nonEmpty, "GUI error: Variable jobschedulerBuildId is empty"))
}
