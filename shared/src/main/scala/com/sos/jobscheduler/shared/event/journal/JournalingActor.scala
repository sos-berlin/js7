package com.sos.jobscheduler.shared.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Stash}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.journal.JournalingActor._
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging {

  private case class Elem(keyedEvent: KeyedEvent[E], callback: Stamped[KeyedEvent[E]] ⇒ Unit)

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  import context.dispatcher

  private var buffer: mutable.Buffer[Elem] = null
  private var afterCallbacks: mutable.Buffer[() ⇒ Unit] = null
  private var timeoutSchedule: Cancellable = null
  private var isPersisting = false

  @Deprecated
  override final def stash(): Nothing =
    throw new UnsupportedOperationException("Don't use stash(). Use journalStash()")

  def journalStash(message: Any): Unit =
    receiveJournalMessage.applyOrElse(message, (_: Any) ⇒ super.stash())

  private[event] final def persistKeyedEvent[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit = {
    startBuffering()
    buffer += Elem(keyedEvent, callback.asInstanceOf[Stamped[KeyedEvent[E]] ⇒ Unit])
  }

  protected final def afterLastPersist(callback: ⇒ Unit): Unit = {
    startBuffering()
    if (afterCallbacks == null) {
      afterCallbacks = mutable.Buffer[() ⇒ Unit]()
    }
    afterCallbacks += { () ⇒ callback }
  }

  private def startBuffering(): Unit = {
    require(!isPersisting)  // Nested calls to persist are not supported
    if (buffer == null) {
      context.become(persisting, discardOld = false)
      self.tell(Internal.Persist, sender())
      buffer = mutable.Buffer[Elem]()
    }
  }

  private val persisting: Receive = {
    case Internal.Persist ⇒
      isPersisting = true
      if (buffer.isEmpty) {
        context.unbecome()
        finishPersisting()
      } else {
        val keyedEvents = (buffer map { _.keyedEvent }).toVector
        journalActor.forward(Journal.Input.Store(keyedEvents, self))
        timeoutSchedule = context.system.scheduler.scheduleOnce(StoreWarnTimeout.toFiniteDuration, self, Internal.TimedOut)
      }

    case Journal.Output.Stored(snapshots) ⇒
      if (timeoutSchedule != null) {
        timeoutSchedule.cancel()
        timeoutSchedule = null
      }
      context.unbecome()
      for ((elem, snapshot) ← buffer zip snapshots) {
        elem.callback(snapshot.asInstanceOf[Stamped[KeyedEvent[E]]])
      }
      buffer = null
      finishPersisting()

    case Internal.TimedOut ⇒
      logger.warn(s"Writing to journal takes longer than ${StoreWarnTimeout.pretty}")

    case _ ⇒
      super.stash()
  }

  final val receiveJournalMessage: Receive = {
    case Input.GetSnapshot ⇒
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) ⇒
          sender ! Output.GotSnapshot(o)
        case Failure(t) ⇒
          val tt = t.appendCurrentStackTrace
          logger.error(t.toString, tt)
          throw tt  // ???
      }

    case msg: Internal ⇒
      crash(msg, s"Unhandled important $msg. context.become/unbecome called in or after persist? Dying.")
  }

  private def finishPersisting(): Unit = {
    buffer = null
    if (afterCallbacks != null) {
      for (o ← afterCallbacks) o()
      afterCallbacks = null
    }
    unstashAll()
    isPersisting = false
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
  private val StoreWarnTimeout = 30.s   // TODO JournalMeta, Configuration
  private val logger = Logger(getClass)

  private sealed trait Internal
  private object Internal {
    case object Persist extends Internal
    case object TimedOut extends Internal
  }

  object Input {
    final case object GetSnapshot
  }

  object Output {
    final case class GotSnapshot(snapshots: Iterable[Any])
    //private[journal] final case object RecoveryFinished
  }
}
