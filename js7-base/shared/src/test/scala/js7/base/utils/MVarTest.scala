package js7.base.utils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite

final class MVarTest extends OurAsyncTestSuite:

  "test" in:
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
