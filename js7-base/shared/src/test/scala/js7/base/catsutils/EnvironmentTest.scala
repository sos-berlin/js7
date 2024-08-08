package js7.base.catsutils

import cats.arrow.FunctionK
import cats.effect.{IO, Resource, Sync, SyncIO}
import cats.syntax.parallel.*
import cats.{effect, ~>}
import izumi.reflect.Tag
import js7.base.catsutils.Environment.environment
import js7.base.catsutils.EnvironmentTest.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isJVM
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

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


  "Concurrent allocation" in:
    // Because allocation is lazy, concurrent reads may allocate concurrently.
    // The second allocation will be released and the first allocation will be used.
    val notReleased = Atomic(0)
    val released = Atomic(0)
    class Q[A](value_ : A) extends AutoCloseable:
      notReleased += 1
      @volatile private var closed = false
      def close() =
        closed = true
        released += 1
        notReleased -= 1
      def value =
        if closed then fail("CLOSED") else value_

    def resource[A](a: A): Resource[IO, Q[A]] =
      Resource.fromAutoCloseable(IO(Q(a)).delayBy(100.ms))

    val registeringResource =
      for
        _ <- Environment.register(resource("hej!"))
        _ <- Environment.register(resource[0](0))
        _ <- Environment.register(resource[1](1))
        _ <- Environment.register(resource[2](2))
        _ <- Environment.register(resource[3](3))
        _ <- Environment.register(resource[4](4))
        _ <- Environment.register(resource[5](5))
        _ <- Environment.register(resource[6](6))
        _ <- Environment.register(resource[7](7))
        _ <- Environment.register(resource[8](8))
        _ <- Environment.register(resource[9](9))
      yield ()

    val n = 100

    def f[A: Tag]: IO[Seq[A]] =
      Seq.fill(n)(()).parTraverse: _ =>
        Environment.environment[Q[A]].map(_.value)

    val expected = Seq(
      f[String],
      f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7], f[8], f[9])

    registeringResource.surround:
      expected.parSequence
        .flatMap: seq =>
          logger.info(s"${released.get} concurrent releases")
          IO(assert:
            notReleased.get == expected.size &&
            /* Some should have been released*/
            released.get > 0 && released.get < seq.flatten.size &&
              seq == ("hej!" +: (0 to 9)).map(o => Seq.fill(n)(o)))
    .map: _ =>
      assert(notReleased.get == 0)


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


object EnvironmentTest:
  private val logger = Logger[this.type]
