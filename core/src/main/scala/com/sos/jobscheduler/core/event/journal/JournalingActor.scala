package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalingActor._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging {

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  import context.dispatcher

  private val callbacks = mutable.ListBuffer[Callback]()
  private var isStashing = false
  private var journalingInhibited = false

  protected def inhibitJournaling(): Unit =
    journalingInhibited = true

  private[event] final def persistAsyncKeyedEvent[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit =
    persistKeyedEvent(keyedEvent, async = true)(callback)

  private[event] final def persistKeyedEvent[EE <: E](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    noSync: Boolean = false,
    async: Boolean = false)(
    callback: Stamped[KeyedEvent[EE]] ⇒ Unit)
  : Unit = {
    if (!async) {
      startCommit()
    }
    logger.trace(s"“$toString” Store ${keyedEvent.key} ${keyedEvent.event.getClass.simpleScalaName}")
    journalActor.forward(JournalActor.Input.Store(Some(keyedEvent) :: Nil, self, timestamp, noSync = noSync))
    callbacks += EventCallback(callback.asInstanceOf[Stamped[KeyedEvent[E]] ⇒ Unit])
  }

  protected final def defer(callback: ⇒ Unit): Unit = {
    startCommit()
    deferAsync(callback)
  }

  protected final def deferAsync(callback: ⇒ Unit): Unit = {
    journalActor.forward(JournalActor.Input.Store(None :: Nil, self, timestamp = None, noSync = false))
    callbacks += Deferred(() ⇒ callback)
  }

  private def startCommit(): Unit = {
    if (journalingInhibited) throw new IllegalStateException("Journaling has been stopped")  // Avoid deadlock when waiting for response of dead JournalActor
    if (!isStashing) {
      isStashing = true
      context.become(journaling, discardOld = false)
    }
  }

  final def journaling: Receive = {
    case JournalActor.Output.Stored(stampedOptions) ⇒
      // sender() is from persistKeyedEvent or deferAsync
      for (stampedOption ← stampedOptions) {
        if (callbacks.isEmpty) {
          val msg = s"Journal Stored message received (duplicate? stash in callback?) without corresponding callback: $stampedOption"
          logger.error(s"“$toString” $msg")
          throw new RuntimeException(msg)
        }
        (stampedOption, callbacks.remove(0)) match {
          case (Some(stamped), EventCallback(callback)) ⇒
            logger.trace(s"“$toString” Stored ${EventId.toString(stamped.eventId)} ${stamped.value.key} ${stamped.value.event.getClass.simpleScalaName} -> $callback")
            callback(stamped.asInstanceOf[Stamped[KeyedEvent[E]]])
          case (None, Deferred(callback)) ⇒
            callback()
          case x ⇒
            sys.error(s"Bad actor state: $x")
        }
      }
      logger.trace(s"“$toString” callbacks=${callbacks.size}")
      if (isStashing && callbacks.isEmpty) {
        isStashing = false
        context.unbecome()
        unstashAll()
      }

    case Input.GetSnapshot ⇒
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) ⇒
          sender ! Output.GotSnapshot(o)
        case Failure(t) ⇒
          val tt = t.appendCurrentStackTrace
          logger.error(s"“$toString” $t", tt)
          throw tt  // ???
      }

    case _ if isStashing ⇒
      super.stash()
  }

  private sealed trait Callback
  private case class EventCallback(callback: Stamped[KeyedEvent[E]] ⇒ Unit) extends Callback
  private case class Deferred(callback: () ⇒ Unit) extends Callback
}

object JournalingActor {
  private val logger = Logger(getClass)

  object Input {
    private[journal] final case object GetSnapshot
  }

  object Output {
    private[journal] final case class GotSnapshot(snapshots: Iterable[Any])
  }
}
