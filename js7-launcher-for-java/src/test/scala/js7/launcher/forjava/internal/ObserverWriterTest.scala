package js7.launcher.forjava.internal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.concurrent.Channel
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.test.OurAsyncTestSuite
import js7.launcher.StdWriter

final class ObserverWriterTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "ObserverWriter" in:
    for
      channel <- Channel.bounded[IO, String](1)
      resultFiber <- channel.stream.compile.toList.start
      _ <- IO.interruptible:
        val w = BlockingStdWriter(StdWriter(channel))
        w.write("EINS")
        w.write('-')
        w.write("ZWEI")
      _ <- channel.close
      result <- resultFiber.joinStd
    yield
      assert(result == List("EINS", "-", "ZWEI") )
