package js7.launcher

import cats.effect.IO
import fs2.concurrent.Channel

/** Writer for a process' stdout and stdin.
 *
 * Forwards chunks of characters to the event writer. */
final class StdWriter(channel: Channel[IO, String]):

  /** Forwards the character chunk to the (asynchronous) event writer.
   *
   * @return false iff channel is closed.
   */
  def write(chunk: String): IO[Boolean] =
    channel.send(chunk).map(_.isRight)
