package js7.base.catsutils

import cats.effect
import cats.effect.{IO, SyncIO}
import js7.base.catsutils.Environment.environment
import js7.base.catsutils.EnvironmentTest.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.system.OperatingSystem.isJVM
import js7.base.test.OurAsyncTestSuite

final class EnvironmentTest extends OurAsyncTestSuite:

  "simple" in:
    ioRuntime.environment.add(X(7))
    for
      _ <- Environment.add(X("Hej!"))
      x <- environment[X[Int]]
      _ = assert(x == X(7))
      x <- environment[X[String]]
      _ = assert(x == X("Hej!"))
    yield succeed

  private val config = config"js7.thread-pools.compute.threads = 2"

  if isJVM then "Each IORuntime has its own associated Environment" in:
    OurIORuntime.resource[SyncIO]("EnvironmentTest", config)
      .use: myRuntime =>
        SyncIO:
          val io =
            for
              _ <- Environment.add(X("Hallå!"))
              x <- environment[X[String]]
              _ = assert(x == X("Hallå!"))
            yield succeed
          io.unsafeRunSync()(using myRuntime)
      .unsafeRunSync()

    for
      x <- environment[X[String]]
      _ = assert(x == X("Hej!"))
    yield succeed


private object EnvironmentTest:
  private final case class X[A](a: A)
