package js7.controller.web.controller.api.log

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.ParameterDirectives._
import akka.http.scaladsl.server.directives.PathDirectives.pathEnd
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import com.typesafe.config.Config
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import js7.base.auth.ValidUserPermission
import js7.base.time.JavaTimeConverters._
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.passIf
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.files.GrowingFileObservable
import js7.common.http.StreamingSupport._
import js7.common.scalautil.Logger
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.log.LogRoute._
import monix.eval.Task
import monix.execution.Scheduler

trait LogRoute extends ControllerRouteProvider
{
  protected def scheduler: Scheduler
  protected def config: Config
  protected def currentLogFile: Path

  private implicit def implicitScheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)
  private lazy val pollDuration = config.getDuration("js7.web.server.services.log.poll-interval").toFiniteDuration

  lazy val logRoute: Route =
    authorizedUser(ValidUserPermission)(_ =>
      pathEnd(
        parameter("snapshot".as[Boolean] ? false) { snapshot =>
          streamFile(currentLogFile, endless = !snapshot)
          //Fails if files grows while read (Content-Length mismatch?): getFromFile(currentLogFile, contentType)
        }))

  private def streamFile(file: Path, endless: Boolean): Route =
    passIf(isRegularFile(file) && isReadable(file))(
      complete(
        HttpEntity(
          contentType,  // Ignore requester's `Accept` header
          new GrowingFileObservable(file, endless ? pollDuration)
            .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
              Task { logger.debug("whenShuttingDown completed") })
            .map(_.toByteString)
            .toAkkaSourceForHttpResponse)))
}

object LogRoute
{
  private val logger = Logger(getClass)
  private val contentType = `text/plain(UTF-8)`
}
