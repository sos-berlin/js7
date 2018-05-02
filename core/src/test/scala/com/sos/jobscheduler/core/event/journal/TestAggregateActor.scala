package com.sos.jobscheduler.core.event.journal

import akka.Done
import akka.actor.ActorRef
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.core.event.journal.TestAggregateActor._

/**
  * @author Joacim Zschimmer
  */
private[journal] final class TestAggregateActor(protected val key: String, val journalActor: ActorRef)
extends KeyedJournalingActor[TestEvent] {

  private var aggregate: TestAggregate = null
  private var disturbance = 0

  protected def snapshot = Option(aggregate)

  protected def recoverFromSnapshot(o: Any) = {
    aggregate = cast[TestAggregate](o)
  }

  protected def recoverFromEvent(event: TestEvent) =
    update(event)

  def receive = journaling orElse {
    case command: Command ⇒
      command match {

        case Command.Disturb(value) ⇒
          disturbance = value

        case Command.DisturbAndRespond ⇒
          deferAsync {
            sender() ! "OK"
          }

        case Command.Add(string) ⇒
          persist(TestEvent.Added(string)) { e ⇒
            update(e)
            sender() ! Done
          }

        case Command.Remove ⇒
          persist(TestEvent.Removed) { e ⇒
            update(e)
            sender() ! Response.Completed(disturbance)
          }

        case Command.Append(string) ⇒
          for (c ← string) {
            persist(TestEvent.Appended(c)) { e ⇒
              update(e)
            }
          }
          defer {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendNoSync(char) ⇒
          persist(TestEvent.Appended(char), noSync = true) { e ⇒
            update(e)
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendAsync(string) ⇒
          for (c ← string) {
            persistAsync(TestEvent.Appended(c))(update)
          }
          deferAsync {
            sender() ! Response.Completed(disturbance)
          }

        case Command.AppendNested(string) ⇒
          def append(string: List[Char]): Unit = string match {
            case char :: tail ⇒
              persist(TestEvent.Appended(char)) { e ⇒
                update(e)
                append(tail)
              }
            case Nil ⇒
              sender() ! Response.Completed(disturbance)
          }
          append(string.toList)

        case Command.AppendNestedAsync(string) ⇒
          def append(string: List[Char]): Unit = string match {
            case char :: tail ⇒
              persistAsync(TestEvent.Appended(char)) { e ⇒
                update(e)
                append(tail)
              }
            case Nil ⇒
              sender() ! Response.Completed(disturbance)
          }
          append(string.toList)
      }

    case Input.Get ⇒
      assert(aggregate != null)
      deferAsync {  // For testing
        sender() ! aggregate
      }
  }


  private def update(event: TestEvent): Unit =
    event match {
      case event: TestEvent.Added ⇒
        assert(aggregate == null)
        import event._
        aggregate = TestAggregate(key, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r)

      case TestEvent.Removed ⇒
        aggregate = null
        context.stop(self)

      case event: TestEvent ⇒
        aggregate = aggregate.applyEvent(event)
    }
}

private[journal] object TestAggregateActor {

  object Input {
    final case object Get
  }

  object Output {
    final case object Ready
  }

  sealed trait Command
  final object Command {
    sealed trait IsAsync
    sealed trait DoNotDisturb
    final case class Disturb(int: Int) extends Command
    final case object DisturbAndRespond extends Command
    final case class Add(string: String) extends Command
    final case class Append(string: String) extends Command
    final case class AppendNoSync(char: Char) extends Command
    final case class AppendAsync(string: String) extends Command with IsAsync
    final case class AppendNested(string: String) extends Command
    final case class AppendNestedAsync(string: String) extends Command with IsAsync
    final case object Remove extends Command with DoNotDisturb
  }

  object Response {
    final case class Completed(disturbance: Int)
  }
}
