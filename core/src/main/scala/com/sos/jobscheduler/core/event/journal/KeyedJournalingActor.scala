package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import scala.collection.immutable.Iterable
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

  protected final def snapshots: Future[Iterable[Any]] =
    Future.successful(snapshot.toList)

  protected final def persist[EE <: E, A](event: EE, noSync: Boolean = false, async: Boolean = false)(callback: EE ⇒ A): Future[A] = {
    registerMe()
    super.persistKeyedEvent(KeyedEvent(key, event), noSync = noSync,  async = async) { stampedEvent ⇒
      callback(stampedEvent.value.event.asInstanceOf[EE])
    }
  }

  override def unhandled(msg: Any) = msg match {
    case Input.RecoverFromSnapshot(o) ⇒
      registered = true
      recoverFromSnapshot(o)

    case Input.RecoverFromEvent(Stamped(_, _, KeyedEvent(k, event))) ⇒
      assert(k == key)
      registered = true
      recoverFromEvent(event.asInstanceOf[E])

    case Input.FinishRecovery ⇒
      callFinishRecovery()
      sender() ! KeyedJournalingActor.Output.RecoveryFinished

    case _ ⇒ super.unhandled(msg)
  }

  private def registerMe(): Unit =
    if (!registered) {
      journalActor ! JournalActor.Input.RegisterMe(Some(key))
      registered = true
    }

  private def callFinishRecovery(): Unit = {
    finishRecovery()
    val snapshot = this.snapshot
    if (snapshot == null) sys.error(s"Actor (${getClass.getSimpleName}) for '$key': snapshot is null")
  }
}

object KeyedJournalingActor {
  object Input {
    final case class RecoverFromSnapshot(snapshot: Any)
    final case class RecoverFromEvent(eventStamped: Stamped[AnyKeyedEvent])
    final case object FinishRecovery
  }

  object Output {
    private[journal] case object RecoveryFinished
  }
}
