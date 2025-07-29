package js7.common.web.serviceprovider

import com.typesafe.config.Config
import js7.base.auth.Permission
import js7.base.log.Logger
import js7.base.system.ServiceProviders.findServices
import js7.base.utils.Collections.implicits.*
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.CommonConfiguration
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.StandardDirectives.combineRoutes
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.web.serviceprovider.ServiceProviderRoute.*
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait ServiceProviderRoute:
  this: RouteProvider =>

  protected def routeServiceContext: RouteServiceContext =
    RouteServiceContext(config)

  protected def config: Config
  protected def commonConf: CommonConfiguration

  private lazy val services: Seq[RouteService] =
    findServices[RouteService]()

  private val lazyServiceProviderRoute = Lazy[Route]:
    val servicePathRoutes: Seq[(RouteService, String, Route, Set[Permission])] =
      for
        service <- services
        routeMapper = service.newRouteMapper(commonConf)
        (path, (route, permissions)) <- routeMapper.pathToRoute(routeServiceContext)
      yield
        (service, path, route, permissions)
    logAndCheck(servicePathRoutes)
    combineRoutes:
      servicePathRoutes.map: (_, path, route, permissions) =>
        pathSegments(path):
          authorizedUser(permissions): _ =>
            route

  /** Routes provided by a Java service. */
  protected final val serviceProviderRoute: Route =
    //implicit val exceptionHandler = ExceptionHandler { case throwable =>
    //  // Do not return exception to client
    //  logger.error(throwable.toStringWithCauses)
    //  if (throwable.getStackTrace.nonEmpty) logger.error(throwable.toStringWithCauses, throwable)
    //  complete(StatusCodes.NotFound, Problem.pure("Unknown URI or RouteService failed"))
    //}
    //Route.seal(
      requestContext =>
        if lazyServiceProviderRoute.isEmpty then
          logger.debug(s"Looking up RouteService for unhandled URI ${requestContext.request.uri.path}")
        lazyServiceProviderRoute()(requestContext)


object ServiceProviderRoute:
  private val logger = Logger[this.type]

  private def logAndCheck(namedRouteToService: Seq[(RouteService, String, Route, Set[Permission])])
  : Unit =
    if namedRouteToService.isEmpty then
      logger.trace("No routes")
    else
      for (routeService, path, _, _) <- namedRouteToService do
        logger.debug(s"${routeService.getClass.scalaName} provides web route '/$path'")

    for pathToTriples <- namedRouteToService.duplicateKeys(_._2) do
      sys.error:
        "Duplicate route paths: " +
          pathToTriples.values.flatten
            .map(o => s"'${o._2}'->${o._1.getClass.scalaName}")
            .mkString(", ")
