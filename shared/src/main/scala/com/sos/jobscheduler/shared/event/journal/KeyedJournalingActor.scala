package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Snapshot}
import java.util.Objects.requireNonNull
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[E <: Event] extends JournalingActor[E] {

  private var registered = false

  protected def key: E#Key
  protected def snapshot: Any
  protected def recoverFromSnapshot(snapshot: Any): Unit
  protected def recoverFromEvent(event: E): Unit

  protected final def snapshots = Future.successful(List(requireNonNull(snapshot)))

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
    case JournalingActor.Input.RecoverFromSnapshot(o) ⇒
      registered = true
      recoverFromSnapshot(o)

    case JournalingActor.Input.RecoverFromEvent(Snapshot(_, KeyedEvent(k, event))) ⇒
      assert(k == key)
      registered = true
      recoverFromEvent(event.asInstanceOf[E])

  //case JournalingActor.Input.FinishRecovery ⇒
  //  finishRecovery()
  //  sender() ! JournalingActor.Output.RecoveryFinished

    case _ ⇒ super.unhandled(msg)
  }
}
