package js7.proxy.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.common.metrics.RemoteMetricsRoute
import js7.common.pekkohttp.PekkoHttpServerUtils.extensions.mapAndRechunkToByteStringBuffered
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import org.apache.pekko.http.scaladsl.model.ContentType
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

trait ProxyMetricsRoute extends RemoteMetricsRoute:

  protected def metrics: fs2.Stream[IO, ByteString]
  private given IORuntime = ioRuntime
  
  protected def deepMetricsRoute(contentType: ContentType): Route =
    completeWithStream(contentType):
      metrics.mapAndRechunkToByteStringBuffered(httpChunkSize)(identity)
