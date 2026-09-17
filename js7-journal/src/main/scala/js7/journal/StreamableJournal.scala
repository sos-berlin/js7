package js7.journal

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeError
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.AtomicStopper
import js7.data.event.EventCalc.given
import js7.data.event.{Event, JournaledState, MaybeTimestampedKeyedEvent, TimeCtx}
import js7.journal.StreamableJournal.*
import js7.journal.{CommitOptions, Journal, Persisted}

trait StreamableJournal[S <: JournaledState[S]](chunkSize: Int):
  this: Journal[S] =>

  def persistStream[E <: Event : Tag](
    stream: fs2.Stream[IO, MaybeTimestampedKeyedEvent[E]],
    stopper: AtomicStopper)
    (surround: (Seq[MaybeTimestampedKeyedEvent[E]], IO[Checked[Persisted[S, E]]]) =>
      IO[Checked[Persisted[S, E]]])
  : IO[Unit] =
    stream.through:
      persistPipe(stopper)(surround)
    .map(_.orThrow)
    .compile.drain

  def persistPipe[E <: Event : Tag](stopper: AtomicStopper)
    (surround: (Seq[MaybeTimestampedKeyedEvent[E]], IO[Checked[Persisted[S, E]]]) =>
      IO[Checked[Persisted[S, E]]])
  : fs2.Pipe[IO, MaybeTimestampedKeyedEvent[E], Checked[Persisted[S, E]]] =
    val stoppedException = PersistStoppedException()
    _.chunkLimit(chunkSize).evalMap: chunk =>
      stopper.resource.use:
        case Some(stopReason) =>
          logger.debug(s"◼️ persistPipe[${Tag[E].tag}] $stopReason")
          IO.raiseError(PersistStoppedException())
        case None =>
          surround(
            chunk.asSeq,
            persist(StreamCommitOptions)(chunk.asSeq))
    .recoverWith:
      case _: PersistStoppedException => fs2.Stream.empty


object StreamableJournal:
  private val logger = Logger[StreamableJournal.type]

  private val StreamCommitOptions = CommitOptions(
    commitLater = true,
    /*delay = subagentConf.stdoutCommitDelay*/)

  private final class PersistStoppedException extends RuntimeException:
    override def toString = "PersistStoppedException"
