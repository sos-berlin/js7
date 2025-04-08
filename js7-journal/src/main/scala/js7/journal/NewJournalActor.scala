package js7.journal

import cats.effect.{IO, Resource, ResourceIO}
import cats.effect.kernel.DeferredSource
import cats.effect.unsafe.IORuntime
import com.softwaremill.tagging.{@@, Tagger}
import izumi.reflect.Tag
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.thread.CatsBlocking.unsafeRunSyncX
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.SupervisorStrategies
import js7.data.Problems.ClusterNodeHasBeenSwitchedOverProblem
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, MaybeTimestampedKeyedEvent, SnapshotableState, Stamped}
import js7.journal.JournalActor.*
import js7.journal.Journaler.Persisted
import js7.journal.configuration.JournalConf
import js7.journal.recover.Recovered
import org.apache.pekko.actor.{Actor, ActorRef, ActorRefFactory, Props, Stash, SupervisorStrategy}
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[S <: SnapshotableState[S]: Tag] private(journaler: Journaler[S])
  (using ioRuntime: IORuntime)
extends Actor, Stash, JournalLogging:

  import context.stop

  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategies.escalate
  protected val conf: JournalConf = journaler.conf

  for o <- conf.simulateSync do logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
  logger.whenTraceEnabled { logger.debug("Logger isTraceEnabled=true") }

  def receive: Receive = receiveGet orElse:
    case Input.Store(correlId, timestamped, replyTo, options, since, commitLater, callersItem) =>
      val sender = this.sender()
      if journaler.isStopping then
        for o <- timestamped do logger.debug:
          s"Event rejected because journal is halted: ${o.keyedEvent.toString.truncateWithEllipsis(200)}"
        // We ignore the event and do not notify the caller,
        // because it would crash and disturb the process of switching-over.
        // (so AgentDriver with AgentReady event)
        reply(sender, replyTo,
          Output.Stored(
            Left(ClusterNodeHasBeenSwitchedOverProblem/*???*/),
            journaler.unsafeUncommittedAggregate(),
            callersItem))
      else
        journaler.enqueue:
          Journaler.Persist(
            EventCalc.add(timestamped.map(_.toMaybeTimestamped)),
            options.copy(commitLater = commitLater),
            since)
        .flatMap: (whenApplied, whenCommitted) =>
          whenApplied.get.map(_.map(_ -> whenCommitted))
        .awaitInfinite match
          case Left(problem) =>
            reply(sender, replyTo,
              Output.Stored(Left(problem), journaler.unsafeUncommittedAggregate(), callersItem))
          case Right((written, whenCommitted)) =>
            if commitLater then
              reply(sender, replyTo, Output.Accepted(callersItem))
            else
              self ! Internal.Written(whenCommitted, sender, replyTo, callersItem)

    case Internal.Written(whenCommitted, sender, replyTo, callersItem) =>
     try
       val committed = whenCommitted.get.logWhenItTakesLonger("whenCommitted in JournalActor")
         .awaitInfinite.orThrow
       reply(sender, replyTo,
         Output.Stored(
           Right(committed.stampedKeyedEvents),
           committed.aggregate,
           callersItem))
     catch case t: Throwable =>
      logger.error(t.toStringWithCauses)
      throw t

    case Input.TakeSnapshot =>
      val sender = this.sender()
      runAsync:
        journaler.takeSnapshot() *> IO: // tryTakeSnapshotIfRequested() ???
          sender ! Output.SnapshotTaken

  private def reply(sender: ActorRef, replyTo: ActorRef, msg: Any): Unit =
    replyTo.!(msg)(sender)

  private def receiveGet: Receive =
    case Input.GetJournalActorState =>
      sender() ! Output.JournalActorState(
        isFlushed = journaler != null && journaler.isFlushed,
        isSynced = journaler != null && journaler.isSynced,
        isRequiringClusterAcknowledgement = journaler.isRequiringClusterAcknowledgement.unsafeRunSyncX())

    case Input.GetJournaledState =>
      // Allow the caller outside of this JournalActor to read committedState
      // asynchronously at any time.
      // Returned function accesses committedState directly and asynchronously !!!
      sender() ! (() => journaler.unsafeAggregate())

    case Input.GetIsHaltedFunction =>
      // Allow the caller outside of this JournalActor to read isHalted
      // asynchronously at any time.
      sender() ! (() => journaler.isHalted)

  private def runAsync(body: IO[Unit])(using file: sourcecode.FileName, line: sourcecode.Line): Unit =
    body.onError: throwable =>
      IO:
        logger.error(s"${throwable.toStringWithCauses} while in ${file.value}:${line.value}", throwable)
        stop(self)
    .unsafeRunAndForget()


  private object Internal:
    final case class Written(
      whenCommitted: DeferredSource[IO, Checked[Persisted[S, Event]]],
      sender: ActorRef,
      replyTo: ActorRef,
      callersItem: CallersItem)


object JournalActor:
  private val logger = Logger[this.type]

  def resource[S <: SnapshotableState[S]: Tag](journaler: Journaler[S])
    (using
      ioRuntime: IORuntime,
      actorRefFactory: ActorRefFactory)
  : ResourceIO[ActorRef @@ JournalActor.type] =
    Resource.make(
      acquire = IO:
        actorRefFactory
          .actorOf(
            Props(new JournalActor[S](journaler)),
            "Journal")
          .taggedWith[JournalActor.type])(
      release = journalActor =>
        logger.debugIO(s"JournalActor[${implicitly[Tag[S]].tag}] stop"):
          IO:
            actorRefFactory.stop(journalActor))

  private[journal] trait CallersItem

  object Input:
    private[journal] final case class Start[S <: SnapshotableState[S]](recovered: Recovered[S])

    private[journal] final case class Store(
      correlId: CorrelId,
      timestamped: Seq[Timestamped],
      journalingActor: ActorRef,
      options: CommitOptions,
      since: Deadline,
      commitLater: Boolean = false,
      callersItem: CallersItem)

    case object TakeSnapshot
    case object GetJournalActorState
    case object GetJournaledState
    case object GetIsHaltedFunction

  private[journal] trait Timestamped:
    def keyedEvent: AnyKeyedEvent
    def timestampMillis: Option[Long]

    def toMaybeTimestamped: MaybeTimestampedKeyedEvent[Event] =
      MaybeTimestampedKeyedEvent(keyedEvent, timestampMillis)

  sealed trait Output
  object Output:
    private[journal] final case class Stored[S <: SnapshotableState[S]](
      stamped: Checked[Seq[Stamped[AnyKeyedEvent]]],
      journaledState: S,
      callersItem: CallersItem)
    extends Output

    private[journal] final case class Accepted(callersItem: CallersItem) extends Output
    case object SnapshotTaken
    final case class JournalActorState(isFlushed: Boolean, isSynced: Boolean, isRequiringClusterAcknowledgement: Boolean)

  type Stopped = Stopped.type
  case object Stopped
