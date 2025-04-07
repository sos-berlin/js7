package js7.journal.test

import cats.effect.unsafe.IORuntime
import com.softwaremill.tagging.@@
import js7.base.generic.Accepted
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.journal.configuration.JournalConf
import js7.journal.test.TestAggregateActor.*
import js7.journal.{JournalActor, KeyedJournalingActor}
import org.apache.pekko.Done
import org.apache.pekko.actor.{ActorRef, Status}
import scala.concurrent.ExecutionContext
import scala.language.unsafeNulls
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[test] final class TestAggregateActor(key_ : String, val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf)
  (implicit protected val ioRuntime: IORuntime)
extends KeyedJournalingActor[TestState, TestEvent]:

  protected def key = key_.asInstanceOf[E.Key]
  private var aggregate: TestAggregate = null
  private var disturbance = 0

  private given ExecutionContext = ioRuntime.compute

  def receive =
    case Input.RecoverFromSnapshot(aggregate) =>
      this.aggregate = aggregate

    case command: Command =>
      command match

        case Command.Disturb(value) =>
          disturbance = value

        case Command.DisturbAndRespond =>
          deferAsync:
            sender() ! "OK"

        case Command.Add(string) =>
          val before = persistedEventId
          persist(TestEvent.Added(string)) { (e, s) =>
            assert(before < persistedEventId)
            update(e)
            sender() ! Done
          }

        case Command.Remove =>
          val before = persistedEventId
          persist(TestEvent.Removed) { (e, s) =>
            assert(before < persistedEventId)
            update(e)
            sender() ! Response.Completed(disturbance)
          }

        case Command.Append(string) =>
          persistTransaction(string map TestEvent.Appended.apply) { (es, journaledState) =>
            es foreach update
          }
          defer:
            sender() ! Response.Completed(disturbance)

        case Command.AppendEmpty =>
          persistKeyedEvents(Nil) { (seq, journaledState) =>
            assert(seq.isEmpty)
            sender() ! Response.Completed(disturbance)
          }

        case Command.AcceptEarly =>
          val sender = this.sender()
          val event = TestEvent.NothingDone
          persistAcceptEarlyIO(event).unsafeToFuture() onComplete:
            // persistAcceptEarlyIO does not update persistedEventId
            case Success(Right(_: Accepted)) => sender ! Response.Completed(disturbance)
            case Success(Left(problem)) =>
              logger.error(problem.toString)
              sender ! Status.Failure(problem.throwable)
            case Failure(t) =>
              logger.error(t.toStringWithCauses, t)
              sender ! Status.Failure(t)

        case Command.AppendAsync(string) =>
          val iterator = string.iterator
          for c <- string do
            val before = persistedEventId
            persist(TestEvent.Appended(c), async = true) { (e, s) =>
              assert(before < persistedEventId)
              assert(c == iterator.next())
              update(e)
            }
          deferAsync:
            sender() ! Response.Completed(disturbance)

        case Command.AppendNested(string) =>
          def append(string: List[Char]): Unit = string match
            case char :: tail =>
              persist(TestEvent.Appended(char)) { (e, s) =>
                update(e)
                append(tail)
              }
            case Nil =>
              sender() ! Response.Completed(disturbance)
          append(string.toList)

        case Command.AppendNestedAsync(string) =>
          def append(string: List[Char]): Unit = string match
            case char :: tail =>
              persist(TestEvent.Appended(char), async = true) { (e, s) =>
                update(e)
                append(tail)
              }
            case Nil =>
              sender() ! Response.Completed(disturbance)
          append(string.toList)

    case Input.Get =>
      assert(aggregate != null)
      deferAsync:  // For testing
        sender() ! aggregate


  private def update(event: TestEvent): Unit =
    event match
      case event: TestEvent.Added =>
        assert(aggregate == null)
        import event.*
        aggregate = TestAggregate(key_, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r)

      case TestEvent.Removed =>
        logger.debug("Removed, stopping now")
        aggregate = null
        context.stop(self)

      case event: TestEvent =>
        aggregate = aggregate.applyEvent(event)

private[journal] object TestAggregateActor:
  private val logger = Logger[this.type]

  object Input:
    final case class RecoverFromSnapshot(snapshot: TestAggregate)
    case object Get

  object Output:
    case object Ready

  sealed trait Command
  object Command:
    sealed trait IsAsync
    final case class Disturb(int: Int) extends Command
    case object DisturbAndRespond extends Command
    final case class Add(string: String) extends Command
    final case class Append(string: String) extends Command
    case object AppendEmpty extends Command
    case object AcceptEarly extends Command
    final case class AppendAsync(string: String) extends Command, IsAsync
    final case class AppendNested(string: String) extends Command
    final case class AppendNestedAsync(string: String) extends Command, IsAsync
    case object Remove extends Command

  object Response:
    final case class Completed(disturbance: Int)
