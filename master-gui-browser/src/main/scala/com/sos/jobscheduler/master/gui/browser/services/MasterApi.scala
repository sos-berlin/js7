package com.sos.jobscheduler.master.gui.browser.services

import com.sos.jobscheduler.common.http.JsHttpClient
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig.buildId

/**
  * @author Joacim Zschimmer
  */
object MasterApi extends HttpMasterApi {
  protected val uri = "/"
  protected val httpClient = new JsHttpClient(buildId.ensuring(_.nonEmpty, "GUI error: Variable jobschedulerBuildId is empty"))
}
