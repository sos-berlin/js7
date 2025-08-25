package js7.base.fs2utils

import cats.effect.IO
import fs2.Pipe

object Fs2Utils:

  type StreamIO[A] = fs2.Stream[IO, A]
  type StreamPure[A] = fs2.Stream[fs2.Pure, A]

  def bytesToLines: Pipe[IO, Byte, String] =
    _.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
