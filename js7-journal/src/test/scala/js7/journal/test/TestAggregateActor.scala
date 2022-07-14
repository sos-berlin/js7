package js7.journal.test

import akka.Done
import akka.actor.{ActorRef, Status}
import js7.base.generic.Accepted
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.journal.configuration.JournalConf
import js7.journal.test.TestAggregateActor.*
import js7.journal.{JournalActor, KeyedJournalingActor}
import monix.execution.Scheduler
import scala.util.{Failure, Success}
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
private[test] final class TestAggregateActor(protected val key: String, val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf)
  (implicit protected val scheduler: Scheduler)
extends KeyedJournalingActor[TestState, TestEvent] {

  private var aggregate: TestAggregate = null
  private var disturbance = 0

  def receive = {
    case Input.RecoverFromSnapshot(aggregate) =>
      this.aggregate = aggregate

    case command: Command =>
      command match {

        case Command.Disturb(value) =>
          disturbance = value

        case Command.DisturbAndRespond =>
          deferAsync {
            sender() ! "OK"
          }

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
          defer {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendEmpty =>
          persistKeyedEvents(Nil) { (seq, journaledState) =>
            assert(seq.isEmpty)
            sender() ! Response.Completed(disturbance)
          }

        case Command.AcceptEarly =>
          val sender = this.sender()
          val event = TestEvent.NothingDone
          persistAcceptEarlyTask(event).runToFuture onComplete {
            // persistAcceptEarlyTask does not update persistedEventId
            case Success(Right(_: Accepted)) => sender ! Response.Completed(disturbance)
            case Success(Left(problem)) =>
              logger.error(problem.toString)
              sender ! Status.Failure(problem.throwable)
            case Failure(t) =>
              logger.error(t.toStringWithCauses, t)
              sender ! Status.Failure(t)
          }

        case Command.AppendAsync(string) =>
          for (c <- string) {
            val before = persistedEventId
            persist(TestEvent.Appended(c), async = true) { (e, s) =>
              assert(before < persistedEventId)
              update(e)
            }
          }
          deferAsync {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendNested(string) =>
          def append(string: List[Char]): Unit = string match {
            case char :: tail =>
              persist(TestEvent.Appended(char)) { (e, s) =>
                update(e)
                append(tail)
              }
            case Nil =>
              sender() ! Response.Completed(disturbance)
          }
          append(string.toList)

        case Command.AppendNestedAsync(string) =>
          def append(string: List[Char]): Unit = string match {
            case char :: tail =>
              persist(TestEvent.Appended(char), async = true) { (e, s) =>
                update(e)
                append(tail)
              }
            case Nil =>
              sender() ! Response.Completed(disturbance)
          }
          append(string.toList)
      }

    case Input.Get =>
      assert(aggregate != null)
      deferAsync {  // For testing
        sender() ! aggregate
      }
  }


  private def update(event: TestEvent): Unit =
    event match {
      case event: TestEvent.Added =>
        assert(aggregate == null)
        import event.*
        aggregate = TestAggregate(key, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r)

      case TestEvent.Removed =>
        logger.debug("Removed, stopping now")
        aggregate = null
        context.stop(self)

      case event: TestEvent =>
        aggregate = aggregate.applyEvent(event)
    }
}

private[journal] object TestAggregateActor
{
  private val logger = Logger(getClass)

  object Input {
    final case class RecoverFromSnapshot(snapshot: TestAggregate)
    final case object Get
  }

  object Output {
    final case object Ready
  }

  sealed trait Command
  final object Command {
    sealed trait IsAsync
    final case class Disturb(int: Int) extends Command
    final case object DisturbAndRespond extends Command
    final case class Add(string: String) extends Command
    final case class Append(string: String) extends Command
    case object AppendEmpty extends Command
    final case object AcceptEarly extends Command
    final case class AppendAsync(string: String) extends Command with IsAsync
    final case class AppendNested(string: String) extends Command
    final case class AppendNestedAsync(string: String) extends Command with IsAsync
    final case object Remove extends Command
  }

  object Response {
    final case class Completed(disturbance: Int)
  }
}
