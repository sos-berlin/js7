package com.sos.jobscheduler.shared.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.journal.JournalingActor._
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

  private[event] final def persistAsyncKeyedEvent[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit =
    persistKeyedEvent(keyedEvent, async = true)(callback)

  private[event] final def persistKeyedEvent[EE <: E](keyedEvent: KeyedEvent[EE], noSync: Boolean = false, async: Boolean = false)(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit = {
    if (!isStashing && !async) {
      isStashing = true
      context.become(journaling, discardOld = false)
    }
    logger.trace(s"($toString) Store ${keyedEvent.key} ${keyedEvent.event.getClass.getSimpleName stripSuffix "$"}")
    journalActor.forward(JsonJournalActor.Input.Store(Some(keyedEvent) :: Nil, self, noSync = noSync))
    callbacks += EventCallback(callback.asInstanceOf[Stamped[KeyedEvent[E]] ⇒ Unit])
  }

  protected final def deferAsync(callback: ⇒ Unit): Unit = {
    journalActor.forward(JsonJournalActor.Input.Store(None :: Nil, self, noSync = false))
    callbacks += Deferred(() ⇒ callback)
  }

  final def journaling: Receive = {
    case JsonJournalActor.Output.Stored(snapshotOptions) ⇒
      // sender() is from persistKeyedEvent or deferAsync
      for (snapshotOption ← snapshotOptions) {
        (snapshotOption, callbacks.remove(0)) match {
          case (Some(snapshot), EventCallback(callback)) ⇒
            logger.trace(s"($toString) Stored ${EventId.toString(snapshot.eventId)} ${snapshot.value.key} -> $callback")
            callback(snapshot.asInstanceOf[Stamped[KeyedEvent[E]]])
          case (None, Deferred(callback)) ⇒
            callback()
          case x ⇒
            sys.error(s"Bad actor state: $x")
        }
      }
      logger.trace(s"($toString) *** callbacks=${callbacks.size}")
      //if (callbacks.isEmpty/*everything is stored*/ && timeoutSchedule != null) {
      //  timeoutSchedule.cancel()
      //  timeoutSchedule = null
      //}
      if (isStashing) {
        isStashing = false
        context.unbecome()
        unstashAll()
      }

    //case Internal.TimedOut ⇒
    //  logger.warn(s"Writing to journal takes longer than ${StoreWarnTimeout.pretty}: $toString")

    case Input.GetSnapshot ⇒
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) ⇒
          sender ! Output.GotSnapshot(o)
        case Failure(t) ⇒
          val tt = t.appendCurrentStackTrace
          logger.error(s"($toString) $t", tt)
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
  //private val StoreWarnTimeout = 30.s
  private val logger = Logger(getClass)

  //private sealed trait Internal
  //private object Internal {
    //case object TimedOut extends Internal
  //}

  object Input {
    private[journal] final case object GetSnapshot
  }

  object Output {
    private[journal] final case class GotSnapshot(snapshots: Iterable[Any])
  }
}
