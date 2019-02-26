package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.generic.Accepted
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

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

  protected final def persist[EE <: E, A](event: EE, async: Boolean = false)(callback: EE => A): Future[A] = {
    registerMe()
    super.persistKeyedEvent(KeyedEvent(key, event), async = async) { stampedEvent =>
      callback(stampedEvent.value.event.asInstanceOf[EE])
    }
  }

  protected final def persistAcceptEarly[EE <: E, A](event: EE, delay: FiniteDuration = Duration.Zero): Future[Accepted] = {
    registerMe()
    super.persistKeyedEventAcceptEarly(KeyedEvent(key, event), delay = delay)
  }

  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: Seq[EE] => A)
  : Future[A] = {
    registerMe()
    super.persistKeyedEvents(events map (e => Timestamped(KeyedEvent(key, e))), async = async, transaction = true) {
      stampedEvents => callback(stampedEvents map (_.value.event.asInstanceOf[EE]))
    }
  }

  override def journaling = super.journaling orElse {
    case Input.RecoverFromSnapshot(o) =>
      registered = true
      recoverFromSnapshot(o)

    case Input.RecoverFromEvent(Stamped(_, _, KeyedEvent(k, event))) =>
      assert(k == key)
      registered = true
      recoverFromEvent(event.asInstanceOf[E])

    case Input.FinishRecovery =>
      callFinishRecovery()
      sender() ! KeyedJournalingActor.Output.RecoveryFinished
  }

  private def registerMe(): Unit =
    if (!registered) {
      journalActor ! JournalActor.Input.RegisterMe
      registered = true
    }

  private def callFinishRecovery(): Unit = {
    finishRecovery()
    val snapshot = this.snapshot
    if (snapshot == null) sys.error(s"Actor (${getClass.getSimpleName}) for '$key': snapshot is null")
  }

  override def toString = s"${getClass.simpleScalaName}($key)"
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
