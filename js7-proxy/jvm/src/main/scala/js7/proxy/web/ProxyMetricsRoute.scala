package js7.proxy.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.common.metrics.RemoteMetricsRoute
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.http.scaladsl.server.{Directives, Route}
import org.apache.pekko.util.ByteString

trait ProxyMetricsRoute extends RemoteMetricsRoute:

  protected def metrics: fs2.Stream[IO, ByteString]
  private given IORuntime = ioRuntime

  protected final lazy val proxyMetricsRoute: Route =
    wrapMetricsRoute: contentType =>
      import Directives.*
      parameter("deep" ? false):
        case false =>
          onlyThisServerMetricsRoute(contentType)
        case true =>
          completeWithStream(contentType):
            metrics
              .map(_.toChunk).unchunks.chunkN(httpChunkSize)
              .map(_.toByteString)
