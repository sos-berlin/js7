package js7.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.apache.pekko.util.ByteString

trait MetricsForServlet:

  def ioRuntime: IORuntime

  def metrics(deep: Boolean): fs2.Stream[IO, ByteString]
