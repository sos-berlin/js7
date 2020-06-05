package js7.master.web.master.api.log

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.ParameterDirectives._
import akka.http.scaladsl.server.directives.PathDirectives.pathEnd
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import js7.base.auth.ValidUserPermission
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.AkkaHttpServerUtils.passIf
import js7.common.akkahttp.StandardMarshallers.logAkkaStreamErrorToWebLogAndIgnore
import js7.common.files.GrowingFileObservable
import js7.common.http.AkkaHttpUtils.AkkaByteVector
import js7.common.http.StreamingSupport._
import js7.common.time.JavaTimeConverters._
import js7.master.web.common.MasterRouteProvider
import js7.master.web.master.api.log.LogRoute._
import com.typesafe.config.Config
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import monix.execution.Scheduler

trait LogRoute extends MasterRouteProvider
{
  protected def scheduler: Scheduler
  protected def config: Config
  protected def currentLogFile: Path

  private lazy val pollDuration = config.getDuration("jobscheduler.webserver.services.log.poll-interval").toFiniteDuration
  implicit private def implicitScheduler = scheduler

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
          logAkkaStreamErrorToWebLogAndIgnore(
            new GrowingFileObservable(file, endless ? pollDuration)
              .map(_.toByteString)
              .toAkkaSource))))
}

object LogRoute
{
  private val contentType = `text/plain(UTF-8)`
}
