package js7.base.catsutils

import cats.arrow.FunctionK
import cats.effect.{IO, Resource, Sync, SyncIO}
import cats.{effect, ~>}
import js7.base.catsutils.Environment.environment
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.system.OperatingSystem.isJVM
import js7.base.test.OurAsyncTestSuite

final class EnvironmentTest extends OurAsyncTestSuite:

  private var closedCounter = 0
  private def xResource[F[_], A](a: A)(using F: Sync[F]) =
    Resource(F.delay:
      X(a) -> F.delay[Unit](closedCounter += 1))

  "register Resource[IO] and Resource[SyncIO]" in:
    val syncToIO: SyncIO ~> IO =
      new FunctionK[SyncIO, IO]:
        def apply[A](syncIO: SyncIO[A]): IO[A] = syncIO.to[IO]

    val registeringResource =
      for
        /// register a Resource[IO] ///
        _ <- Environment.register(xResource[IO, String]("hej!"))
        _ <- Environment.register(xResource[IO, Int](7))
        /// register a Resource[SyncIO] ///
        _ <- OurIORuntimeRegister.toEnvironment(ioRuntime)
          .register(xResource[SyncIO, Boolean](true))
          .mapK(syncToIO)
      yield ()

    registeringResource.surround:
      val program =
        for
          hej <- environment[X[String]]
          seven <- environment[X[Int]]
          bool <- environment[X[Boolean]]
        yield
          assert(hej == X("hej!") && seven == X(7) && bool.a && closedCounter == 0)
      program.map: _ =>
        assert(closedCounter == 0)
    .map: _ =>
      assert(closedCounter == 3)


  if isJVM then "Each IORuntime has its own associated Environment" in:
    val config = config"js7.thread-pools.compute.threads = 2"
    Environment.register(xResource[IO, String]("OUTER IORuntime"))
      .surround:
        OurIORuntime.resource[IO]("EnvironmentTest", config)
          .use: myRuntime =>
            IO:
              SyncIO:
                Environment.register(xResource[IO, String]("INNER"))
                  .surround:
                    environment[X[String]].map: x =>
                      assert(x == X("INNER"))
                  .unsafeRunSync()(using myRuntime)
          .flatMap: _ =>
            environment[X[String]].map(x => assert(x == X("OUTER IORuntime")))
        .flatMap: _ =>
          environment[X[String]].map(x => assert(x == X("OUTER IORuntime")))
      .flatMap: _ =>
        environment[X[String]].attempt.map(x => assert(x.isLeft))

  private final case class X[A](a: A)
