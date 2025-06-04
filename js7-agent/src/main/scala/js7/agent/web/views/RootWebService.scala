package js7.agent.web.views

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentRouteProvider
import js7.base.BuildInfo
import js7.base.system.SystemInformations.systemInformation
import js7.base.system.startup.StartUp
import js7.common.pekkohttp.CirceJsonSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.system.JavaInformations.javaInformation
import org.apache.pekko.http.scaladsl.model.headers.CacheDirectives.`max-age`
import org.apache.pekko.http.scaladsl.model.headers.`Cache-Control`
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentRouteProvider:

  // Overridable for testing
  protected def agentOverview(): AgentOverview =
    AgentOverview(
      startedAt = StartUp.startedAt,
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      system = systemInformation(),
      java = javaInformation())

  private given IORuntime = ioRuntime

  protected final lazy val apiRootRoute: Route =
    pathEnd:
      get:
        respondWithHeader(`Cache-Control`(`max-age`(0))):
          completeIO:
            IO(agentOverview())
