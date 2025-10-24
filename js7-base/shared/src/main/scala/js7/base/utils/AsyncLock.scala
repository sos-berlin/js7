package js7.base.utils

import cats.effect.Resource.ExitCase
import cats.effect.std.Mutex
import cats.effect.{FiberIO, IO, Resource, ResourceIO}
import js7.base.catsutils.CatsEffectExtensions.{defer, startAndCatchError}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, LogLevel, Logger}
import js7.base.monixutils.SimpleLock
import js7.base.scalasource.ScalaSourceLocation
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.{Deadline, FiniteDuration}

trait AsyncLock:
  def resource(acquirer: => String): ResourceIO[Unit]

  final def resource(using src: sourcecode.Enclosing, loc: ScalaSourceLocation): ResourceIO[Unit] =
    resource(toAcquirerLabel(src.value, loc))

  final def lock[A](io: IO[A])(using src: sourcecode.Enclosing, loc: ScalaSourceLocation): IO[A] =
    lock(src.value)(io)

  final def lock[A](acquirer: => String)(body: IO[A])(using loc: ScalaSourceLocation): IO[A] =
    resource(toAcquirerLabel(acquirer, loc))
      .surround(body)

  private def toAcquirerLabel(acquirer: String, loc: ScalaSourceLocation) =
    s"$acquirer${acquirer.nonEmpty ?? " "}at $loc"


object AsyncLock:

  private val logger = Logger[this.type]
  private val AlwaysLogMinor = sys.props.contains("js7.AsyncLock.logMinor")
  private val counter = Atomic(0)

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
      var name_ = name
      if name.contains(' ') then name_ = s"'$name'"
      new WithLogging(name_, logWorryDurations, logMinor = logMinor | AlwaysLogMinor)

  def dontLog(): AsyncLock =
    new NoLogging("AsyncLock")


  private final class NoLogging(name: String) extends AsyncLock:
    private val mutex = memoize(Mutex[IO])

    def resource(acquirer: => String): ResourceIO[Unit] =
      Resource.eval(mutex).flatMap(_.lock)

    override def toString = s"AsyncLock:$name"


  private[utils] final class WithLogging(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration],
    logMinor: Boolean)
  extends AsyncLock:
    private val mutex = SimpleLock[IO]
    private val stateMutex = SimpleLock[IO]
    private val stateVar = Atomic[State](EmptyState)

    @TestOnly
    private[utils] def isLocked: Boolean =
      stateVar.get.isInstanceOf[LockedState]

    private def queueLength: Long =
      stateVar.get.queueLength

    def resource(acquirer: => String): ResourceIO[Unit] =
      for
        onAcquired <- logAcquisition(acquirer)
        _ <- mutex.resource
        _ <- Bean.locked.countConcurrency[IO]
        _ <- Resource.eval(onAcquired)
      yield ()

    private def logAcquisition(acquirer: => String): ResourceIO[IO[Unit]] =
      Resource.defer:
        val sym = new BlockingSymbol
        val since = Deadline.now
        val nr = counter.incrementAndGet()
        lazy val nrString = s"†$nr"
        lazy val acquirer_ = acquirer
        var acquisitionMinorLogged = false
        var acquired = false
        var firstLogged = !logMinor

        def logBeforeAcquire: IO[Unit] =
          IO.never
            .whenItTakesLonger(View(ZeroDuration) ++ logWorryDurations): _ =>
              IO:
                if !firstLogged then
                  logger.trace(s"⚪️$nrString $name is being acquired by $acquirer_ ...")
                  firstLogged = true
                else
                  sym.onInfo()
                  logger.info:
                    s"$sym$nrString $name: $acquirer_ is still waiting for ${since.elapsed.pretty} ($queueLength queuing)..."
            .onCancel:
              IO:
                if !acquired && sym.used then
                  logger.log(sym.logLevel,
                    s"◼️ $nrString $name acquisition canceled after ${since.elapsed.pretty} ↙")

        def logAfterAcquire: IO[Unit] =
          IO:
            if sym.used then
              logger.log(sym.relievedLogLevel,
                s"↘ 🟢$nrString $name acquired by $acquirer_ · $queueLength queuing · ${
                  since.elapsed.pretty} ↘")
            //else
            //  acquisitionMinorLogged = true
            //  logger.trace(s"↘ ⚪️$nrString $name acquired by $acquirer_ · $queueLength queuing · ${
            //    since.elapsed.pretty} ↘")

        def logRelease(exitCase: ExitCase): IO[Unit] =
          IO:
            if acquisitionMinorLogged || sym.used then
              val logLevel = if sym.used then sym.logLevel else LogLevel.Trace
              exitCase match
                case ExitCase.Succeeded =>
                  logger.log(logLevel,
                    s"↙ ⚪️$nrString $name released by $acquirer_ · $queueLength queuing · ${
                      since.elapsed.pretty} ↙")

                case ExitCase.Canceled =>
                  logger.log(logLevel,
                    s"↙ ◼️ $nrString $name released by $acquirer_ · Canceled · $queueLength queuing · ${
                      since.elapsed.pretty} ↙")

                case ExitCase.Errored(t) =>
                  logger.log(sym.logLevel,
                    s"↙ 💥$nrString $name released by $acquirer_ · $queueLength queuing · ${
                      since.elapsed.pretty} · ${t.toStringWithCauses} ↙")

        def logAcquisitionResource =
          Resource
            .makeCase(
              acquire =
                logBeforeAcquire.startAndCatchError)(
              release = (fiber, exitCase) =>
                fiber.cancel *> logRelease(exitCase))
            .evalMap: fiber =>
              acquired = true
              // Return an IO to be called after the lock has been acquired
              fiber.cancel

        for
          _ <- Bean.queued.countConcurrency[IO]
          _ <- logWaitingResource(acquirer_, nr)
          _ <- if logMinor then logAcquisitionResource else Resource.unit
          _ <- Resource.onFinalize(IO(Bean.usedTotal += 1))
        yield
          acquired = true
          // Return an IO to be called after the lock has been acquired
          logAfterAcquire
    end logAcquisition

    private def logWaitingResource(acquirer: String, nr: Long): Resource[IO, Unit] =
      Resource.makeCase(
        acquire =
          stateMutex.surround:
            IO.defer:
              val since = Deadline.now
              stateVar.get match
                case EmptyState =>
                  IO:
                    stateVar := NotQueueingState(acquirer, nr, since)

                case state: NotQueueingState =>
                  IO.defer:
                    var relieveLogLevel: LogLevel = LogLevel.None
                    val sym = new BlockingSymbol
                    IO.never.whenItTakesLonger(logWorryDurations): _ =>
                      IO:
                        stateVar.get match
                          case EmptyState => // unexpected
                          case _: NotQueueingState => // unexpected
                          case QueueingState(currentAcquirer, currentNr, acquiredSince, queueLength, _) =>
                            sym.escalateUpTo(2)
                            relieveLogLevel = sym.relievedLogLevel
                            logger.log(sym.logLevel, s"$sym $name is continuously acquired since ${
                              since.elapsed.pretty
                            }, currently by $currentAcquirer †$currentNr since ${
                              acquiredSince.elapsed.pretty
                            } · $queueLength queuing")
                    .guarantee:
                      IO:
                        stateVar.get match
                          case EmptyState => // unexpected
                          case NotQueueingState(currentAcquirer, nr, acquiredSince) => // unexpected
                            logger.log(relieveLogLevel,
                              s"🟢 $name released by $currentAcquirer †$nr after ${
                                acquiredSince.elapsed.pretty}")
                          case QueueingState(currentAcquirer, nr, acquiredSince, queueLength, _) =>
                            logger.log(relieveLogLevel,
                              s"🟢 $name released by $currentAcquirer †$nr after ${
                                acquiredSince.elapsed.pretty}, $queueLength queuing")
                  .startAndCatchError.map: fiber =>
                    stateVar := state.toQueueingState(fiber)

                case state: QueueingState =>
                  IO:
                    stateVar := state.incrementQueue)(

        release = (_, exitCase) =>
          stateMutex.surround:
            IO.defer:
              stateVar.get match
                case EmptyState => IO.pure(EmptyState) // unexpected

                case state: NotQueueingState =>
                  IO:
                    stateVar := EmptyState

                case state: QueueingState =>
                  if state.queueLength > 1 then
                    IO:
                      stateVar := state.decrementQueue
                  else
                    state.loggingFiber.cancel *>
                      IO:
                        stateVar := state.toNotQueueingState)

    override def toString = s"AsyncLock:$name"


    private sealed trait State:
      def queueLength: Long

    private case object EmptyState extends State:
      val queueLength = 0L


    private sealed trait LockedState extends State

    private final case class NotQueueingState(
      currentAcquirer: String,
      currentNr: Long,
      acquiredSince: Deadline)
    extends LockedState:
      def queueLength = 0

      def toQueueingState(loggingFiber: FiberIO[Unit]): QueueingState =
        QueueingState(currentAcquirer, currentNr, acquiredSince, queueLength = 1, loggingFiber)


    private final case class QueueingState(
      currentAcquirer: String,
      currentNr: Long,
      acquiredSince: Deadline,
      queueLength: Long,
      loggingFiber: FiberIO[Unit])
    extends LockedState:
      def incrementQueue: QueueingState =
        copy(queueLength = queueLength + 1)

      def decrementQueue: QueueingState =
        assert(queueLength > 1)
        copy(queueLength = queueLength - 1)

      def toNotQueueingState: NotQueueingState =
        assert(queueLength == 1)
        NotQueueingState(currentAcquirer, currentNr, acquiredSince)

  end WithLogging


  sealed trait AsyncLockMXBean:
    def getLockedCount: Int
    def getQueuedCount: Int
    def getUsedTotal: Long

  object Bean extends AsyncLockMXBean:
    // locked and queued may be inconsistent while updated
    private[AsyncLock] val locked = Atomic(0)
    private[AsyncLock] val queued = Atomic(0)
    private[AsyncLock] val usedTotal = Atomic(0L)
    def getLockedCount: Int = locked.get
    def getQueuedCount: Int = queued.get // (queued.get - locked.get) max 0 // Not synchronized !!!
    def getUsedTotal: Long = usedTotal.get
