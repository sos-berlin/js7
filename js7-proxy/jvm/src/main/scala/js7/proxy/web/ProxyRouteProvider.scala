package js7.proxy.web

import io.circe.Encoder
import js7.common.pekkohttp.web.session.{RouteProvider, SimpleSession}

trait ProxyRouteProvider extends RouteProvider:
  protected type OurSession = SimpleSession
  protected val sessionEncoder = summon[Encoder.AsObject[SimpleSession]]
