package js7.proxy.web

import cats.effect.unsafe.IORuntime
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.common.metrics.RemoteMetricsRoute
import js7.common.pekkohttp.PekkoHttpServerUtils
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.proxy.ControllerApi
import org.apache.pekko.http.scaladsl.server.Directives.parameter
import org.apache.pekko.http.scaladsl.server.{Directives, Route}
import org.apache.pekko.util.ByteString

trait ProxyMetricsRoute extends RemoteMetricsRoute:

  protected def controllerApi: ControllerApi
  private given IORuntime = ioRuntime

  protected final lazy val proxyMetricsRoute: Route =
    wrapMetricsRoute: contentType =>
      import Directives.*
      parameter("onlyThisServer" ? false):
        case true =>
          metricsRawRoute(contentType)
        case false =>
          PekkoHttpServerUtils.completeWithStream(contentType):
            fs2.Stream.force:
              controllerApi.metrics.map:
                case Left(problem) =>
                  fs2.Stream.emit:
                    ByteString(s"# ERROR ${
                      problem.toString.truncateWithEllipsis(1000, firstLineOnly = true)}\n")
                case Right(stream) =>
                  stream.map(_.toByteString)
