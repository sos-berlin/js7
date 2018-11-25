package com.sos.jobscheduler.master.gui.server

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.StandardDirectives._
import com.sos.jobscheduler.common.utils.JavaResource

/**
  * @author Joacim Zschimmer
  */
trait WebjarsRoute
{
  private lazy val webjarsResourceDirectory = JavaResource("META-INF/resources/webjars")

  /** Resource directory for copied (by build.sbt) Webjars files, to avoid distribution of big webjars. */
  protected def copiedWebjarsResource: JavaResource

  protected final val webjarsRoute: Route =
    get {
      immutableResource {
        getFromResourceDirectory(copiedWebjarsResource.path) ~
          getFromResourceDirectory(webjarsResourceDirectory.path)  // Artifacts of Maven groupId 'org.webjars'
      }
    }
}
