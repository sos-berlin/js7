package js7.base.io.file.watch

import cats.data.NonEmptySeq
import cats.effect
import cats.effect.kernel.Deferred
import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{IO, Outcome}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import fs2.{Pipe, Stream}
import java.io.IOException
import java.nio.file.Files.size
import java.nio.file.Path
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsDeadline.now
import js7.base.catsutils.UnsafeMemoizable.{memoize, unsafeMemoize}
import js7.base.io.file.watch
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.time.ScalaTime.*
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.continueWithLast
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*
import scala.util.Try

/** A special delay line for DirectoryEvent to detect slow writing of a file as a single FileAdded event.
  *
  * <ul>
  * <li> FileAdded is delayed,
  * <li> FileModified delays FileAdded further,
  * <li> FileDeleted is not delayed (and thus forcing a maybe pending FileAdded).
  * </ul>
  */
private final class DirectoryEventDelayer(
  directory: Path,
  delay: FiniteDuration,
  logDelays: NonEmptySeq[FiniteDuration])
extends Pipe[IO, DirectoryEvent, DirectoryEvent]:

  import Operation.*

  private val feed = new Feed
  private val maturer = new Maturer

  def apply(in: Stream[IO, DirectoryEvent]): Stream[IO, DirectoryEvent] =
    Stream
      .bracket(
        acquire = run(in).start)(
        release = _.joinWithUnit
          .handleError(t => logger.error(s"💥 ${t.toStringWithCauses}")))
      .*>(feed.stream)

  private def run(in: Stream[IO, DirectoryEvent]): IO[Unit] =
    for
      eating <- eatAll(in).start
      maturing <- maturer.emitMaturedFileAddedEvents.start
      _ <- IO.both(
        eating.join.flatMap:
          case Outcome.Canceled() => maturing.cancel
          case Outcome.Errored(t) => feed.error(t) *> maturer.finish
          case Outcome.Succeeded(_) => maturer.finish,
        maturing.join.flatMap:
          case Outcome.Canceled() => feed.finish
          case Outcome.Errored(t) => eating.cancel *> feed.error(t)
          case Outcome.Succeeded(_) => feed.finish)
    yield ()

  private def eatAll(in: Stream[IO, DirectoryEvent]): IO[Unit] =
    logger.debugIO:
      in.foreach(eatEvent).compile.drain

  private def eatEvent(directoryEvent: DirectoryEvent): IO[Unit] =
    directoryEvent match
      case fileAdded @ FileAdded(path) =>
        maturer.remove(path) *>
        maturer.addAddedFile(fileAdded)

      case fileModified: FileModified =>
        maturer.onFileModified(fileModified)

      case fileDeleted @ FileDeleted(path) =>
        maturer.remove(path).flatMap:
          if _ then
            // FileDeleted before FileAdded has been delayed completely: we ignore the file
            IO.unit
          else
            feed.emit(fileDeleted)


  private final class Feed:
    private val out = Queue.unbounded[IO, Either[Throwable, DirectoryEvent]].unsafeMemoize
    private val eof = null.asInstanceOf[Either[Throwable, DirectoryEvent]]

    def emit(event: watch.DirectoryEvent): IO[Unit] =
      emit_(Right(event))

    def error(throwable: Throwable): IO[Unit] =
      IO.defer:
        // An error after finish or another error gets lost, so we log it
        logger.debug(s"💥 Feed.error ${throwable.toStringWithCauses}")
        emit_(Left(throwable))

    def finish: IO[Unit] =
      logger.traceIO("Feed.finish"):
        emit_(eof)

    private def emit_(event: Either[Throwable, DirectoryEvent]): IO[Unit] =
      out.flatMap(_.offer(event))

    def stream: Stream[IO, DirectoryEvent] =
      logger.traceStream:
        Stream.eval(out).flatMap(Stream
          .fromQueueUnterminated(_, limit = 100)
          .takeWhile(_ ne eof)
          .flatMap:
            case Left(t) => Stream.raiseError[IO](t)
            case Right(event) => Stream.emit(event))

  /** Delays (matures) FileAdded events and emits them. */
  private final class Maturer:
    private val finishIt = Deferred.unsafe[IO, Unit]
    private val atomicCell = memoize:
      AtomicCell[IO].of(State(TreeMap.empty, Map.empty, Deferred.unsafe))

    private def update(f: State => State): IO[Unit] =
      atomicCell.flatMap(_.update(f))

    private def modify[A](f: State => (State, A)): IO[A] =
      atomicCell.flatMap(_.modify(f))

    def addAddedFile(fileAdded: FileAdded): IO[Unit] =
      for
        now <- CatsDeadline.now
        untilAdded <- modify: state =>
          val updated = state.addFileAdded(fileAdded, now)
          updated -> updated.untilAdded
        _ <- untilAdded.complete(()).void
      yield ()

    def remove(path: Path): IO[Boolean] =
      modify(_.remove(path))

    def onFileModified(fileModified: FileModified): IO[Unit] =
      for
        now <- CatsDeadline.now
        exists <- modify(_.onFileModified(fileModified, now))
        _ <- IO.unlessA(exists)(feed.emit(fileModified))
      yield ()

    def finish: IO[Unit] =
      logger.traceIO("Maturer.finish"):
        finishIt.complete(()).void

    def emitMaturedFileAddedEvents: IO[Unit] =
      logger.debugIO:
        ().tailRecM(_ =>
          for
            now <- CatsDeadline.now
            either <- modify(_.nextOperation(now)).flatMap:
              // Left: loop again, Right: end loop
              case UntilAdded(untilAdded) =>
                IO.race(untilAdded, finishIt.get)

              case Wait(remaining) =>
                IO.sleep(remaining).as(Left(()))

              case Emit(fileAdded) =>
                feed.emit(fileAdded).as(Left(()))
          yield either)
  end Maturer

  private case class State(
    indexToEntry: TreeMap[Long, Entry],
    pathToIndex: Map[Path, Long],
    untilAdded: Deferred[IO, Unit]):

    def addFileAdded(fileAdded: FileAdded, now: CatsDeadline): State =
      addEntry(Entry(fileAdded, now))

    private def addEntry(entry: Entry): State =
      val index = indexToEntry.lastOption.fold(0L)(_._1) + 1
      copy(
        indexToEntry = indexToEntry.updated(index, entry),
        pathToIndex = pathToIndex.updated(entry.path, index))

    def remove(path: Path): (State, Boolean) =
      pathToIndex
        .get(path)
        .flatMap(i => indexToEntry.get(i).map(i -> _)) match
          case None => this -> false
          case Some((i, entry)) =>
            //logger.traceIO(s"remove ${entry.fileAdded}")
            copy(
              pathToIndex = pathToIndex.removed(path),
              indexToEntry = indexToEntry.removed(i)
            ) -> true

    def onFileModified(fileModified: FileModified, now: CatsDeadline): (State, Boolean) =
      pathToIndex.get(fileModified.relativePath) match
        case None => this -> false
        case Some(i) =>
          val entry = indexToEntry(i)
          entry.logStillWritten(now)
          entry.delayUntil = now + delay
          remove(fileModified.relativePath)._1
            .addEntry(entry)
            -> true

    def nextOperation(now: CatsDeadline): (State, Operation) =
      indexToEntry.headOption match
        case None =>
          val until = Deferred.unsafe[IO, Unit]
          copy(untilAdded = until) -> UntilAdded(until.get)

        case Some((index, entry)) =>
          val remaining = entry.delayUntil - now
          if remaining.isPositive then
            this -> Wait(remaining)
          else
            entry.onMatured(now)
            val updated = copy(
              pathToIndex = pathToIndex.removed(entry.path),
              indexToEntry = indexToEntry.removed(index))
            updated -> Emit(entry.fileAdded)
  end State

  private final class Entry(val fileAdded: FileAdded, since: CatsDeadline):
    var delayUntil = since + delay
    private val logDelayIterator = continueWithLast(logDelays)
    private var lastLoggedAt = since
    private var lastLoggedSize = Try(size(directory.resolve(path))) getOrElse 0L
    private var logDelay = delay + logDelayIterator.next()
    private val sym = BlockingSymbol()

    def path = fileAdded.relativePath

    def logStillWritten(now: CatsDeadline): Unit =
      if (now - (lastLoggedAt + logDelay)).isZeroOrAbove then
        logDelay = logDelayIterator.next()
        lastLoggedAt = now
        sym.increment()
        sym.onInfo()
        logger.log(sym.logLevel,
          s"$sym Watched file is still being modified for ${logExtra(now)}")

    def onMatured(now: CatsDeadline) =
      logger.log(sym.releasedLogLevel,
        s"🟢 Watched file is considered written after ${logExtra(now)}")

    private def logExtra(now: CatsDeadline): String =
      val file = directory.resolve(path)
      val growth =
        try
          val s = size(file)
          val growth = s - lastLoggedSize
          lastLoggedSize = s
          " +" + toKBGB(growth)
        catch case _: IOException => ""
      s"${(now - since).pretty}: $file$growth"

    override def toString = s"$path delayUntil=$delayUntil"


object DirectoryEventDelayer:
  private val logger = Logger[this.type]

  def apply(
    directory: Path,
    delay: FiniteDuration,
    logDelays: NonEmptySeq[FiniteDuration])
  : Pipe[IO, DirectoryEvent, DirectoryEvent] =
    if delay.isPositive then
      new DirectoryEventDelayer(directory, delay, logDelays)
    else
      identity


  object syntax:
    extension(self: Stream[IO, DirectoryEvent])
      @deprecated("Use .through(DirectoryEventDelayer.pipe(...))", "v2.7")
      def delayFileAdded(
        directory: Path, delay: FiniteDuration, logDelays: NonEmptySeq[FiniteDuration])
      : Stream[IO, DirectoryEvent] =
        self.through(DirectoryEventDelayer(directory, delay, logDelays))


  private sealed trait Operation
  private object Operation:
    case class UntilAdded(untilAdded: IO[Unit]) extends Operation

    case class Wait(duration: FiniteDuration) extends Operation:
      override def toString = s"Wait(${duration.pretty})"

    case class Emit(fileAdded: FileAdded) extends Operation
