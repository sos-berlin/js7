package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichEither, RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.event.journal.JournalActor.{EventsHeader, SnapshotsHeader}
import com.sos.jobscheduler.core.event.journal.JournalMeta.Header
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Files.exists
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.concurrent.blocking
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[E <: Event] {

  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[AnyKeyedEvent], Unit]

  import journalMeta.{convertInputStream, eventJsonCodec, snapshotJsonCodec}

  private val stopwatch = new Stopwatch
  private var lastEventId: EventId = EventId.BeforeFirst
  private var snapshotCount = 0
  private var eventCount = 0
  private lazy val logger = Logger.withPrefix[JournalRecoverer[_]](journalFile.getFileName.toString)
  lazy val hasJournal = exists(journalFile)

  final def recoverAll(): Unit = {
    if (hasJournal) {
      try
        blocking {  // May take a long time
          logger.info(s"Recovering from journal journalFile '$journalFile' (${toMB(Files.size(journalFile))})")
          autoClosing(new JsonFileIterator(Header, convertInputStream(_, journalFile), journalFile)) { jsonIterator ⇒
            var separator: Option[Json] = jsonIterator.hasNext option jsonIterator.next()
            if (separator contains SnapshotsHeader) {
              separator = recoverJsonsUntilSeparator(jsonIterator, recoverSnapshotJson)
            }
            if (separator contains EventsHeader) {
              separator = recoverJsonsUntilSeparator(jsonIterator, recoverEventJson)
            }
            for (jsValue ← separator) {
              sys.error(s"Unexpected JSON value in '$journalFile': $jsValue")
            }
          }
        }
      catch {
        case t: Exception if journalMeta.isIncompleteException(t) ⇒
          logger.info(s"Journal has not been completed. " + errorClause(t))
        case t: Exception if journalMeta.isCorruptException(t) ⇒
          logger.warn(s"Journal is corrupt or has not been completed. " + errorClause(t))
      }
      logSomething()
    } else {
      logger.info(s"No journal journalFile '$journalFile' left")
    }
  }

  @tailrec
  private def recoverJsonsUntilSeparator(jsonIterator: Iterator[Json], recover: Json ⇒ Unit): Option[Json] =
    if (jsonIterator.hasNext) {
      val json = jsonIterator.next()
      if (json.isObject) {
        recover(json)
        recoverJsonsUntilSeparator(jsonIterator, recover)
      } else
        Some(json)
    } else
      None

  private def recoverSnapshotJson(json: Json): Unit = {
    snapshotCount += 1
    val snapshot = snapshotJsonCodec.decodeJson(json).force
    recoverSnapshot.getOrElse(snapshot,
      sys.error(s"Unrecoverable snapshot in journal journalFile '$journalFile': $snapshot"))
  }

  private def recoverEventJson(json: Json): Unit = {
    eventCount += 1
    val stamped = json.as[Stamped[KeyedEvent[E]]].force
    if (stamped.eventId <= lastEventId)
      sys.error(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(stamped.eventId)} follows ${EventId.toString(lastEventId)}")
    lastEventId = stamped.eventId
    try recoverEvent.getOrElse(stamped, sys.error("Not handled"))
    catch { case NonFatal(t) ⇒
      throw new RuntimeException(s"Unrecoverable event in journal '$journalFile': $stamped: $t", t)
    }
  }

  private def errorClause(t: Throwable) =
    s"Assuming sudden termination, using $snapshotCount snapshots and $eventCount events until ${EventId.toString(lastEventId)}. ${t.toStringWithCauses}"

  private def logSomething(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.info(stopwatch.itemsPerSecondString(snapshotCount + eventCount, "snapshots+events") + " read")
    }
    if (eventCount > 0) {
      logger.info(s"Recovered last EventId is ${EventId.toString(lastEventId)} ($snapshotCount snapshots and $eventCount events read)")
    }
  }
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors)

          def receive = {
            case JournalActor.Output.Ready ⇒
              for (a ← actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinished from ${actors.size} actors")
              becomeWaitingForChildren(actors.size)
          }

          private def becomeWaitingForChildren(n: Int): Unit = {
            if (n == 0) {
              logger.debug(s"JournalIsReady")
              context.parent ! Output.JournalIsReady
              context.stop(self)
            } else {
              context.become {
                case KeyedJournalingActor.Output.RecoveryFinished ⇒
                  logger.trace(s"${n - 1} actors left: Actor has RecoveryFinished: ${actorToKey(sender())}'")
                  becomeWaitingForChildren(n - 1)

                case msg if actorToKey contains sender() ⇒
                  context.parent.forward(msg)  // For example OrderActor.Output.RecoveryFinished
              }
            }
          }
        }
      },
      name = "JournalActorRecoverer")
  }

  object Output {
    case object JournalIsReady
  }
}
