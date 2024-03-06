package js7.base.utils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import scala.util.Random

final class MVarTest extends OurAsyncTestSuite:

  "MVar operations" in:
    for
      mvar <- MVar.empty[IO, Int]
      _ <- mvar.tryRead.map(o => assert(o == None))
      _ <- mvar.put(1)
      _ <- mvar.tryRead.map(o => assert(o == Some(1)))
      _ <- mvar.tryRead.map(o => assert(o == Some(1)))
      _ <- mvar.read.map(o => assert(o == 1))
      _ <- mvar.read.map(o => assert(o == 1))
      _ <- mvar.take.map(o => assert(o == 1))
      _ <- mvar.tryRead.map(o => assert(o == None))
    yield succeed

  "put is cancelable" in:
    val test =
      for
        mvar <- MVar.empty[IO, Int]
        _ <- mvar.put(1)
        // Cancel the put operation
        _ <- mvar.put(2).background.surround(IO.defer(IO.sleep(Random.nextInt(1000).µs)))
        value <- mvar.take
      yield
        assert(value == 1)

    test.replicateA_(1000).as(succeed)
