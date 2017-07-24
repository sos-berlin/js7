package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
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
  protected def finishRecovery() = {}

  protected final def snapshots = Future.successful(snapshot.toList)

  protected final def persistAsync[EE <: E](event: EE)(callback: EE ⇒ Unit): Unit =
    persist(event, async = true)(callback)

  protected final def persist[EE <: E](event: EE, async: Boolean = false)(callback: EE ⇒ Unit): Unit = {
    registerMe()
    super.persistKeyedEvent(KeyedEvent(key, event), async = async) { snapshotEvent ⇒
      callback(snapshotEvent.value.event.asInstanceOf[EE])
    }
  }

  override def unhandled(msg: Any) = msg match {
    case Input.Recover(snapshot) ⇒
      recoverFromSnapshot(snapshot)
      callFinishRecovery()

    case Input.RecoverFromSnapshot(o) ⇒
      registered = true
      recoverFromSnapshot(o)

    case Input.RecoverFromEvent(Stamped(_, KeyedEvent(k, event))) ⇒
      assert(k == key)
      registered = true
      recoverFromEvent(event.asInstanceOf[E])

    case Input.FinishRecovery ⇒
      callFinishRecovery()
      sender() ! KeyedJournalingActor.Output.RecoveryFinished

    case _ ⇒ super.unhandled(msg)
  }

  private def registerMe(): Unit = {
    if (!registered) {
      journalActor ! JsonJournalActor.Input.RegisterMe(Some(key))
      registered = true
    }
  }

  private def callFinishRecovery(): Unit = {
    finishRecovery()
    val snapshot = this.snapshot
    if (snapshot == null) sys.error(s"Actor (${getClass.getSimpleName}) for '$key': snapshot is null")
  }
}

object KeyedJournalingActor {
  object Input {
    private[journal] final case class RecoverFromSnapshot(snapshot: Any)
    private[journal] final case class RecoverFromEvent(eventStamped: Stamped[AnyKeyedEvent])
    private[journal] final case object FinishRecovery
    final case class Recover(snapshot: Any)
  }

  object Output {
    private[journal] case object RecoveryFinished
  }
}
