package js7.base.fs2utils

import cats.effect.IO
import fs2.Pipe

object Fs2Utils:

  def bytesToLines: Pipe[IO, Byte, String] =
    _.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
