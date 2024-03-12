package js7.controller.web.controller.api.log

import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import js7.base.auth.ValidUserPermission
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.common.files.ByteSeqFileReader
import js7.common.files.ByteSeqFileReader.fileStream
import js7.common.pekkohttp.PekkoHttpServerUtils
import js7.common.pekkohttp.PekkoHttpServerUtils.{completeWithByteStream, passIf}
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.log.LogRoute.*
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.ParameterDirectives.*
import org.apache.pekko.http.scaladsl.server.directives.PathDirectives.pathEnd

trait LogRoute extends ControllerRouteProvider:

  protected def ioRuntime: IORuntime
  protected def config: Config
  protected def currentLogFile: Path

  private given IORuntime = ioRuntime

  private lazy val pollDuration = config.getDuration("js7.web.server.services.log.poll-interval").toFiniteDuration

  lazy val logRoute: Route =
    authorizedUser(ValidUserPermission)(_ =>
      pathEnd(
        parameter("snapshot".as[Boolean] ? false) { snapshot =>
          streamFile(currentLogFile, endless = !snapshot)
          //Fails if files grows while read (Content-Length mismatch?): getFromFile(currentLogFile, contentType)
        }))

  private def streamFile(file: Path, endless: Boolean): Route =
    passIf(isRegularFile(file) && isReadable(file)):
      completeWithByteStream(contentType):
        fileStream(file, endless ? pollDuration, fromEnd = endless)
          .interruptWhenF(shutdownSignaled)


object LogRoute:
  private val contentType = `text/plain(UTF-8)`
