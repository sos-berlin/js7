package js7.controller.web.serviceprovider

import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import java.util.ServiceLoader
import js7.base.log.Logger
import js7.base.utils.Collections.implicits.*
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.StandardDirectives.combineRoutes
import js7.controller.web.serviceprovider.ServiceProviderRoute.*
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
private[web] trait ServiceProviderRoute
{
  protected def injector: Injector
  protected def routeServiceContext: RouteServiceContext

  private lazy val services: Seq[RouteService] = {
    val serviceLoader = ServiceLoader.load(classOf[RouteService])

    val iterator = serviceLoader.iterator.asScala
    if (iterator.isEmpty)
      logger.debug("No RouteService")
    else
      for (service <- iterator/*loads services lazily*/) {
        logger.debug(s"Found service provider ${service.getClass.scalaName}")
        injector.injectMembers(service)
      }

    serviceLoader.asScala.toVector
  }

  private val lazyServiceProviderRoute = Lazy[Route] {
    val servicePathRoutes: Seq[(RouteService, String, Route)] =
      for {
        service <- services
        (p, r) <- service.pathToRoute(routeServiceContext)
      } yield (service, p, r)
    logAndCheck(servicePathRoutes)
    combineRoutes(
      for ((_, p, r) <- servicePathRoutes) yield pathSegments(p)(r))
  }

  protected final def serviceProviderRoute: Route = {
    //implicit val exceptionHandler = ExceptionHandler { case throwable =>
    //  // Do not return exception to client
    //  logger.error(throwable.toStringWithCauses)
    //  if (throwable.getStackTrace.nonEmpty) logger.error(throwable.toStringWithCauses, throwable)
    //  complete(StatusCodes.NotFound, Problem.pure("Unknown URI or RouteService failed"))
    //}
    //Route.seal(
      requestContext => {
        if (lazyServiceProviderRoute.isEmpty) {
          logger.debug(s"Looking up RouteService for unhandled URI ${requestContext.request.uri.path}")
        }
        lazyServiceProviderRoute()(requestContext)
      }
  }
}

private[web] object ServiceProviderRoute
{
  private val logger = Logger(getClass)

  private def logAndCheck(namedRouteToService: Seq[(RouteService, String, Route)]): Unit = {
    if (namedRouteToService.isEmpty) logger.trace(s"No routes")
    else for ((s, p, _) <- namedRouteToService) logger.debug(s"${s.getClass.scalaName} provides route '/$p'")
    for (pathToTriples <- namedRouteToService.duplicateKeys(_._2)) {
      sys.error("Duplicate route paths: " + pathToTriples.values.flatten.map(o => s"'${o._2}'->${o._1.getClass.scalaName}" ).mkString(", "))
    }
  }
}
