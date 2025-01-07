package js7.launcher

import cats.effect.{Deferred, IO, Resource, ResourceIO}
import js7.base.io.process.{Stderr, Stdout}
import js7.base.time.ScalaTime.*
import js7.launcher.StdObservers.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

@TestOnly
object StdObserversForTest:

  extension (x: StdObservers.type)
    @TestOnly
    def testSink(
      charBufferSize: Int = 8192,
      chunkSize: Int = 8192,
      delay: FiniteDuration = 100.ms,
      useErrorLineLengthMax: Option[Int] = None,
      name: String)
    : ResourceIO[TestSink] =
      for
        out <- Resource.eval(Deferred[IO, String])
        err <- Resource.eval(Deferred[IO, String])
        outErrToSink: OutErrToSink = Map(
          Stdout -> (_.foldMonoid.evalMap(out.complete).drain),
          Stderr -> (_.foldMonoid.evalMap(err.complete).drain))
        stdObservers <- resource(outErrToSink, charBufferSize, chunkSize, delay,
          useErrorLineLengthMax = useErrorLineLengthMax,
          name = name)
      yield
        TestSink(
          stdObservers,
          out = stdObservers.closeChannels *> out.get,
          err = stdObservers.closeChannels *> err.get)


  /** Provides an OutErrToSink which collects stdout and stderr each in a String. */
  @TestOnly
  final class TestSink private[StdObserversForTest](
    val stdObservers: StdObservers,
    /** out and err implicitly close the channels via closeChannels. */
    val out: IO[String],
    val err: IO[String])
