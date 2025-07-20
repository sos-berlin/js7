package js7.base.fs2utils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class StreamQueueTest extends OurAsyncTestSuite:

  "StreamQueue" in:
    final case class V(key: Int, string: String)

    StreamQueue[Int, V](_.key).flatMap: streamQueue =>
      val send =
        for
          _ <- streamQueue.enqueue(V(1, "A"))
          _ <- IO.sleep(10.ms)
          _ <- streamQueue.enqueue(V(2, "B"))
          _ <- streamQueue.enqueue(V(3, "C"))
          _ <- streamQueue.close
        yield ()

      val receive = streamQueue.stream.compile.toList

      IO.both(send, receive)
        .map(_._2)
        .map: list =>
          assert(list == List(V(1, "A"), V(2, "B"), V(3, "C")))
