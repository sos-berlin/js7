package js7.base.utils

import cats.effect.Resource.ExitCase
import cats.effect.std.Mutex
import cats.effect.{IO, Resource, ResourceIO}
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, LogLevel, Logger}
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.{Deadline, FiniteDuration}

trait AsyncLock:
  final def lock[A](io: IO[A])(implicit src: sourcecode.Enclosing): IO[A] =
    lock(src.value)(io)

  def lock[A](acquirer: => String)(io: IO[A]): IO[A]


object AsyncLock:

  private val logger = Logger[this.type]
  private val dontUse = sys.props.contains("js7.noAsyncLock")
  private val waitCounter = Atomic(0)

  def apply()(using sourcecode.Enclosing): AsyncLock =
    apply(logMinor = false)

  def apply(logMinor: Boolean)(using enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value, logMinor = logMinor)

  def apply(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration] = Worry.Default.durations,
    suppressLog: Boolean = false,
    logMinor: Boolean = false)
  : AsyncLock =
    if suppressLog || dontUse then
      new NoLogging(name)
    else
      new WithLogging(name, logWorryDurations, logMinor = logMinor)

  def dontLog(): AsyncLock =
    new NoLogging("AsyncLock")


  private final class NoLogging(name: String) extends AsyncLock:
    private val mutex = Mutex[IO].unsafeMemoize

    def lock[A](acquirer: => String)(io: IO[A]): IO[A] =
      mutex.flatMap(_.lock.surround(io))

    override def toString = s"AsyncLock:$name"


  private[utils] final class WithLogging(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration],
    logMinor: Boolean)
  extends AsyncLock:
    private val mutex = Mutex[IO].unsafeMemoize
    private val queueLength = Atomic(0)

    @TestOnly
    private[utils] def isLocked: Boolean =
      queueLength.get() > 0

    override def lock[A](acquirer: => String)(body: IO[A]): IO[A] =
      mutex.flatMap: mutex =>
        logging(acquirer).use: onAcquired =>
          mutex.lock.surround:
            onAcquired *> body


    private def logging(acquirer: => String): ResourceIO[IO[Unit]] =
      Resource.defer:
        val since = Deadline.now

        val nr = waitCounter += 1
        lazy val nrString = s"†$nr"
        lazy val acquirer_ = acquirer
        val sym = new BlockingSymbol
        var minorRequestLogged = false
        var acquired = false
        var firstLogged = !logMinor

        def logBeforeAcquire: IO[Unit] =
          IO.never
            .whenItTakesLonger(logMinor.thenView(ZeroDuration) ++ logWorryDurations)(_ => IO:
              if !firstLogged then
                if logMinor then
                  logger.trace(s"⚪️$nrString $name is being acquired by $acquirer_ ...")
                firstLogged = true
              else
                sym.onInfo()
                logger.info:
                  s"$sym$nrString $name: $acquirer_ is still waiting for ${since.elapsed.pretty
                  } ($queueLength queued)...")
            .onCancel(IO:
              if !acquired && sym.used then
                logger.log(sym.logLevel,
                  s"◼️ $nrString $name acquisition canceled after ${since.elapsed.pretty} ↙"))

        def logAfterAcquire: IO[Unit] =
          IO:
            if sym.used then
              logger.log(sym.relievedLogLevel,
                s"↘ 🟢$nrString $name acquired by $acquirer_ · $queueLength queued · ${
                  since.elapsed.pretty} ↘")
            else if logMinor then
              minorRequestLogged = true
              logger.trace(s"↘ ⚪️$nrString $name acquired by $acquirer_ · $queueLength queued · ${
                since.elapsed.pretty} ↘")

        def logRelease(exitCase: ExitCase): IO[Unit] =
          IO:
            if minorRequestLogged || sym.used then
              val logLevel = if sym.used then sym.logLevel else LogLevel.Trace
              exitCase match
                case ExitCase.Succeeded =>
                  logger.log(logLevel,
                    s"↙ ⚪️$nrString $name released by $acquirer_ · $queueLength queued · ${since.elapsed.pretty} ↙")

                case ExitCase.Canceled =>
                  logger.log(logLevel,
                    s"↙ ◼️ $nrString $name released by $acquirer_ · Canceled · $queueLength queued · ${since.elapsed.pretty} ↙")

                case ExitCase.Errored(t) =>
                  logger.log(sym.logLevel,
                    s"↙ 💥$nrString $name released by $acquirer_ · $queueLength queued · ${t.toStringWithCauses} ↙")

        Resource
          .makeCase(
            acquire =
              IO(queueLength += 1) *> logBeforeAcquire.start)(
            release = (fiber, exitCase) =>
              IO(queueLength -= 1) *> fiber.cancel *> logRelease(exitCase))
          .map: fiber =>
            acquired = true
            // Return an IO to be called after the lock has been acquired
            fiber.cancel *> logAfterAcquire

    override def toString = s"AsyncLock:$name"
  end WithLogging
