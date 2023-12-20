package js7.base.utils

import cats.effect.IO
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite

final class MVarTest extends OurAsyncTestSuite:

  "test" in:
    for
      mvar <- MVar.empty[IO, Int]
      _ <- mvar.tryRead.map(o => assert(o == None))
      _ <- IO(Logger[this.type].info(s"### put"))
      _ <- mvar.put(1)
      _ <- IO(Logger[this.type].info(s"### tryRead 1"))
      _ <- mvar.tryRead.map(o => assert(o == Some(1)))
      _ <- IO(Logger[this.type].info(s"### tryRead 2"))
      _ <- mvar.tryRead.map(o => assert(o == Some(1)))
      _ <- IO(Logger[this.type].info(s"### read 1"))
      _ <- mvar.read.map(o => assert(o == 1))
      _ <- IO(Logger[this.type].info(s"### read 2"))
      _ <- mvar.read.map(o => assert(o == 1))
      _ <- IO(Logger[this.type].info(s"### take"))
      _ <- mvar.take.map(o => assert(o == 1))
      _ <- IO(Logger[this.type].info(s"### tryRead"))
      _ <- mvar.tryRead.map(o => assert(o == None))
    yield succeed
