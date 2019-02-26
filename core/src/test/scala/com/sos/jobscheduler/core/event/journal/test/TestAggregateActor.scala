package com.sos.jobscheduler.core.event.journal.test

import akka.Done
import akka.actor.{ActorRef, Status}
import com.sos.jobscheduler.base.generic.Accepted
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichThrowable, cast}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.core.event.journal.test.TestAggregateActor._
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[test] final class TestAggregateActor(protected val key: String, val journalActor: ActorRef)
extends KeyedJournalingActor[TestEvent] {

  import context.dispatcher

  private var aggregate: TestAggregate = null
  private var disturbance = 0

  protected def snapshot = Option(aggregate)

  protected def recoverFromSnapshot(o: Any) = {
    aggregate = cast[TestAggregate](o)
  }

  protected def recoverFromEvent(event: TestEvent) =
    update(event)

  def receive = {
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
          persist(TestEvent.Added(string)) { e =>
            assert(before < persistedEventId)
            update(e)
            sender() ! Done
          }

        case Command.Remove =>
          val before = persistedEventId
          persist(TestEvent.Removed) { e =>
            assert(before < persistedEventId)
            update(e)
            sender() ! Response.Completed(disturbance)
          }

        case Command.Append(string) =>
          persistTransaction(string map TestEvent.Appended.apply)(es => es foreach update)
          defer {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AcceptEarly =>
          val sender = this.sender()
          val event = TestEvent.NothingDone
          persistAcceptEarly(event) onComplete {
            // persistAcceptEarly does not update persistedEventId
            case Success(_: Accepted) => sender ! Response.Completed(disturbance)
            case Failure(t) =>
              logger.error(t.toStringWithCauses, t)
              sender ! Status.Failure(t)
          }

        case Command.AppendAsync(string) =>
          for (c <- string) {
            val before = persistedEventId
            persist(TestEvent.Appended(c), async = true) { event =>
              assert(before < persistedEventId)
              update(event)
            }
          }
          deferAsync {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendNested(string) =>
          def append(string: List[Char]): Unit = string match {
            case char :: tail =>
              persist(TestEvent.Appended(char)) { e =>
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
              persist(TestEvent.Appended(char), async = true) { e =>
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
        import event._
        aggregate = TestAggregate(key, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r)

      case TestEvent.Removed =>
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
