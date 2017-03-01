package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.event.journal.KeyedJournalingActor._
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[E <: Event] extends JournalingActor[E] {

  private var registered = false

  protected def key: E#Key
  protected def snapshot: Option[Any]
  protected def recoverFromSnapshot(snapshot: Any): Unit
  protected def recoverFromEvent(event: E): Unit
  protected def finishRecovery() = ()

  protected final def snapshots = Future.successful(snapshot.toList)

  protected final def persist[EE <: E](event: EE)(callback: EE ⇒ Unit): Unit = {
    if (!registered) {
      journalActor ! Journal.Input.RegisterMe(Some(key))
      registered = true
    }
    super.persistKeyedEvent(KeyedEvent(key, event)) { snapshotEvent ⇒
      callback(snapshotEvent.value.event.asInstanceOf[EE])
    }
  }

  override def unhandled(msg: Any) = msg match {
    case Input.RecoverFromSnapshot(o) ⇒
      registered = true
      recoverFromSnapshot(o)

    case Input.RecoverFromEvent(Snapshot(_, KeyedEvent(k, event))) ⇒
      assert(k == key)
      registered = true
      recoverFromEvent(event.asInstanceOf[E])

  //case Input.FinishRecovery ⇒
  //  finishRecovery()
  //  sender() ! JournalingActor.Output.RecoveryFinished

    case _ ⇒ super.unhandled(msg)
  }
}

object KeyedJournalingActor {
  private[journal] object Input {
    final case class RecoverFromSnapshot(snapshot: Any)
    final case class RecoverFromEvent(eventSnapshot: Snapshot[AnyKeyedEvent])
    //final case object FinishRecovery
  }
}
