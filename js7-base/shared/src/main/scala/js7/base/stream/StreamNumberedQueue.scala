package js7.base.stream

import cats.effect.IO
import cats.syntax.apply.*
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.monixutils.Latch
import js7.base.problem.{Checked, Problem}
import js7.base.stream.StreamNumberedQueue.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.UnsafeMutex

final class StreamNumberedQueue[V: Tag]:

  private val vName = implicitly[Tag[V]].tag.toString
  private val sync = new IncreasingNumberSync(initial = 0, i => s"#$i")
  private val mutex = UnsafeMutex[IO]
  private val stopped = new Latch
  @volatile private var _state = State()

  def enqueue(commands: Iterable[V]): IO[Unit] =
    mutex.lock(IO:
      val s = _state
      var queue = s.queue
      var nextNumber = s.nextNumber
      var lastNumber = -1L
      for command <- commands do
        queue :+= Numbered(nextNumber, command)
        lastNumber = nextNumber
        nextNumber += 1
      _state = s.copy(queue = queue, nextNumber = nextNumber)
      if lastNumber != -1 then
        sync.onAdded(lastNumber))

  def enqueueNumbered(commands: Iterable[Numbered[V]]): IO[Unit] =
    IO.whenA(commands.nonEmpty):
      mutex.lock(IO:
        val s = _state
        var queue = s.queue
        var nextNumber = s.nextNumber
        var lastNumber = -1L
        for command <- commands do
          assertThat(command.number >= nextNumber)
          queue :+= command
          lastNumber = command.number
          nextNumber = command.number + 1
        _state = s.copy(queue = queue, nextNumber = nextNumber)
        if lastNumber != -1 then
          sync.onAdded(lastNumber))

  def stream: Stream[IO, Numbered[V]] =
    stream2(after = _state.torn)

  def stream(after: Long): Stream[IO, Numbered[V]] =
    Stream.suspend:
      _state.checkAfter(after).orThrow
      stream2(after)

  private def stream2(after: Long): Stream[IO, Numbered[V]] =
    Stream.unfoldChunkEval(after): after =>
      IO
        .race(
          left = stopped.when,
          right = sync.whenAvailable(after = after, until = None) *>
            IO(_state.readQueue(after)))
        .flatMap:
          case Left(()) => IO.none
          case Right(Left(StoppedProblem)) => IO.none
          case Right(Left(problem)) => IO.raiseError(problem.throwable)

          case Right(Right(values)) =>
            if values.isEmpty then // Race condition ???
              logger.warn:
                s"Internal: sync.whenAvailable($after) triggered but no command available - delay 1s"
              IO.some(Chunk.empty -> after).delayBy(1.s)
            else
              IO.some(Chunk.from(values) -> values.last.number)

  def release(after: Long): IO[Checked[Unit]] =
    mutex.lock(IO:
      val s = _state
      if s.stopped then
        Checked.unit
      else
        for _ <- s.checkAfter(after) yield
          val (index, found) = s.search(after)
          _state = s.copy(torn = after, queue = s.queue.drop(index + found.toInt)))

  def stop: IO[Vector[Numbered[V]]] =
    mutex.lock:
      stopped.switch *>
        IO:
          val s = _state
          val result = s.queue
          _state = s.stop
          result

  // Unused
  private def dequeueSelected(predicate: V => Boolean): IO[Vector[Numbered[V]]] =
    mutex.lock(IO:
      val s = _state
      val (result, remaining) = s.queue.partition(numbered => predicate(numbered.value))
      _state = s.copy(queue = remaining)
      result)

  override def toString = s"StreamNumberedQueue[$vName]"

  private sealed case class State(
    torn: Long = 0L,
    nextNumber: Long = 1L,
    queue: Vector[Numbered[V]] = Vector.empty[Numbered[V]],
    stopped: Boolean = false):
    def stop: State =
      copy(
        stopped = true,
        queue = Vector.empty)

    def readQueue(after: Long): Checked[Vector[Numbered[V]]] =
      if stopped then
        Left(StoppedProblem)
      else
        val (index, found) = search(after)
        if !found && after != torn then
          Left(unknownAfterProblem(after))
        else
          Right(queue.drop(index + found.toInt))

    def requireValidNumber(after: Long): IO[Unit] =
      IO.defer:
        val last = queue.lastOption.map(_.number)
        IO.raiseWhen(after < torn || last.exists(_ < after)):
          unknownAfterProblem(after).throwable

    def checkAfter(after: Long): Checked[Unit] =
      notStopped *>
        ((beforeFirstIndex <= after && after <= lastIndex) !!
          Problem.pure:
            s"Unknown number: Numbered[$vName]: #$after (must be >=$beforeFirstIndex and <=$lastIndex)")

    def unknownAfterProblem(after: Long) =
      Problem.pure:
        s"Unknown number: Numbered[$vName]: #$after (must be >=$beforeFirstIndex and <=$lastIndex)"

    private def beforeFirstIndex: Long =
      queue.headOption.map(_.number - 1) getOrElse torn

    private def lastIndex: Long =
      queue.lastOption.map(_.number) getOrElse torn

    def notStopped: Checked[this.type] =
      if stopped then
        Left(StoppedProblem)
      else
        Right(this)

    def search(after: Long): (Int, Boolean) =
      binarySearch(queue, _.number)(after)
  end State


object StreamNumberedQueue:
  private val logger = Logger[this.type]
  private val StoppedProblem = Problem.pure("StreamNumberedQueue stopped")
