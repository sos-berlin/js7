package com.sos.scheduler.engine.shared.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.shared.event.journal.JournalingActor._
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging {

  protected type EventSnapshot = Snapshot[KeyedEvent[E]]
  private case class Elem(keyedEvent: KeyedEvent[E], callback: Snapshot[KeyedEvent[E]] ⇒ Unit)

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  private var buffer = mutable.Buffer[Elem]()
  private var isPersisting = false
  private implicit val executionContext = context.dispatcher

  private[event] final def persistKeyedEvent[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Snapshot[KeyedEvent[EE]] ⇒ Unit): Unit = {
    require(!isPersisting)  // Nested calls to persist are not supported
    if (buffer.isEmpty) {
      context.become(persisting, discardOld = false)
      self.tell(Persist, sender())
    }
    buffer += Elem(keyedEvent, callback.asInstanceOf[Snapshot[KeyedEvent[E]] ⇒ Unit])
  }

  private val persisting: Receive = {
    case Persist ⇒
      isPersisting = true
      val keyedEvents = (buffer map { _.keyedEvent }).toVector
      journalActor.forward(Journal.Input.Store(keyedEvents, self))

    case Journal.Output.Stored(snapshots) ⇒
      for ((elem, snapshot) ← buffer zip snapshots) {
        elem.callback(snapshot.asInstanceOf[Snapshot[KeyedEvent[E]]])
      }
      buffer.clear()
      context.unbecome()
      unstashAll()
      isPersisting = false

    case _ ⇒
      stash()
  }

  final val receiveJournalMessage: Receive = {
    case Input.GetSnapshot ⇒
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) ⇒
          sender ! Output.GotSnapshot(o)
        case Failure(t) ⇒
          logger.error(t.toString, t)
          throw t  // ???
      }

    case msg: Internal ⇒
      crash(msg, s"Unhandled important $msg. context.become/unbecome called in or after persist? Dying.")
  }

  private def crash(message: Any, error: String): Unit = {
    logger.error(error + (buffer map { _.keyedEvent }).mkString("\n", "\n", ""))
    super.unhandled(message)
    throw new IllegalStateException(error)
  }

  /**
    * Overriding method must forward to here, or receiveJournalMessage must be called.
    */
  override def unhandled(message: Any): Unit =
    receiveJournalMessage.applyOrElse(message, super.unhandled)
}

object JournalingActor {
  private val logger = Logger(getClass)

  private sealed trait Internal
  private case object Persist extends Internal

  object Input {
    private[journal] final case class RecoverFromSnapshot(snapshot: Any)
    private[journal] final case class RecoverFromEvent(eventSnapshot: Snapshot[AnyKeyedEvent])
    //private[journal] final case object FinishRecovery
    final case object GetSnapshot
  }

  object Output {
    final case class GotSnapshot(snapshots: Iterable[Any])
    //private[journal] final case object RecoveryFinished
  }
}
