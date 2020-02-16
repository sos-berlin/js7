package com.sos.jobscheduler.agent.web.master

import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment

trait MasterRoute extends MastersEventRoute
{
  protected final lazy val masterRoute =
    pathSegment("event") {
      masterEventRoute
    }
}
