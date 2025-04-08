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
  def resource(acquirer: => String): ResourceIO[Unit]

  final def resource(implicit src: sourcecode.Enclosing): ResourceIO[Unit] =
    resource(src.value)

  final def lock[A](io: IO[A])(implicit src: sourcecode.Enclosing): IO[A] =
    lock(src.value)(io)

  final def lock[A](acquirer: => String)(body: IO[A]): IO[A] =
    resource(acquirer).surround(body)


object AsyncLock:

  private val logger = Logger[this.type]
  private val waitCounter = Atomic(0)

  def apply()(using sourcecode.Enclosing): AsyncLock =
    apply(logMinor = false)

  def apply(logMinor: Boolean)(using enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value, logMinor = logMinor)

  def supressLog()(using enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value, suppressLog = true)

  def apply(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration] = Worry.Default.durations,
    suppressLog: Boolean = false,
    logMinor: Boolean = false)
  : AsyncLock =
    if suppressLog then
      new NoLogging(name)
    else
      new WithLogging(name, logWorryDurations, logMinor = logMinor)

  def dontLog(): AsyncLock =
    new NoLogging("AsyncLock")


  private final class NoLogging(name: String) extends AsyncLock:
    private val mutex = Mutex[IO].unsafeMemoize

    def resource(acquirer: => String): ResourceIO[Unit] =
      Resource.eval(mutex).flatMap(_.lock)

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

    def resource(acquirer: => String): ResourceIO[Unit] =
      Resource.eval(mutex).flatMap: mutex =>
        logging(acquirer).flatMap: onAcquired =>
          mutex.lock.evalMap: _ =>
            onAcquired

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
