package js7.controller.web.common

import io.circe.Encoder
import js7.common.pekkohttp.web.session.{RouteProvider, SimpleSession}

/**
  * @author Joacim Zschimmer
  */
trait ControllerRouteProvider extends RouteProvider:

  protected type OurSession = SimpleSession
  protected val sessionEncoder = summon[Encoder.AsObject[SimpleSession]]
