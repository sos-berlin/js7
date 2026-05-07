package js7.common.metrics

import cats.effect.IO
import js7.data.node.Js7ServerId
import org.apache.pekko.util.ByteString

/** Wrapped IO that fetches Prometheus metrics for a Js7ServerId.
  */
final case class MetricFetcher(js7ServerId: Js7ServerId, stream: fs2.Stream[IO, ByteString]):
  def toPair = (js7ServerId, stream)

