package com.sos.jobscheduler.core.event.journal.test

import akka.actor.{ActorContext, ActorRef}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.core.event.journal.data.RecoveredJournalingActors
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalActorRecoverer extends JournalRecoverer[TestState]
{
  protected implicit def sender: ActorRef
  protected def recoverNewKey: PartialFunction[Stamped[AnyKeyedEvent], Unit]
  protected def snapshotToKey: Any => Any
  protected def isDeletedEvent: Event => Boolean
  protected def newJournalEventWatch: JournalEventWatch

  private var state = TestState.empty
  private val keyToActor = mutable.Map[Any, ActorRef]()

  final def recoverAllAndTransferTo(journalActor: ActorRef)(implicit context: ActorContext): Unit = {
    recoverAll()
    startJournalAndFinishRecovery(journalActor = journalActor, state, recoveredJournalingActors,
      Some(newJournalEventWatch))
  }

  protected final def recoverEvent = {
    case stamped @ Stamped(_, _, keyedEvent @ KeyedEvent(key, event)) =>
      state = state.applyEvent(keyedEvent).orThrow
      keyToActor.get(key) match {
        case None =>
          recoverNewKey.getOrElse(stamped,
            sys.error(s"Uncoverable event for a new key in journal '${journalFileOption getOrElse journalMeta.fileBase}': $stamped"))
        case Some(a) =>
          a ! KeyedJournalingActor.Input.RecoverFromEvent(stamped)   // TODO OutOfMemoryError
          if (isDeletedEvent(event.asInstanceOf[Event])) {
            keyToActor -= key
          }
        }
  }

  protected def recoverActorForSnapshot(snapshot: Any, actorRef: ActorRef): Unit = {
    state = state.applySnapshot(snapshot)
    val key = snapshotToKey(snapshot)
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate snapshot in journal journalFile: '$key'")
    keyToActor += key -> actorRef
    actorRef ! KeyedJournalingActor.Input.RecoverFromSnapshot(snapshot)
  }

  protected def recoverActorForNewKey(stampedEvent: Stamped[AnyKeyedEvent], actorRef: ActorRef): Unit = {
    val keyedEvent = stampedEvent.value
    import keyedEvent.key
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate key: '$key'")
    keyToActor += key -> actorRef
    actorRef ! KeyedJournalingActor.Input.RecoverFromEvent(stampedEvent)
  }

  private[journal] final def recoveredJournalingActors: RecoveredJournalingActors =
    RecoveredJournalingActors(keyToActor.toMap)
}
