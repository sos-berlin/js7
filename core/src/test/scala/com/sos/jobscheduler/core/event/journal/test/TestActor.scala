package com.sos.jobscheduler.core.event.journal.test

import akka.Done
import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator}
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.test.TestActor._
import com.sos.jobscheduler.core.event.journal.test.TestJsonCodecs.TestKeyedEventJsonCodec
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
private[journal] final class TestActor(journalMeta: JournalMeta[TestEvent], journalStopped: Promise[JournalActor.Stopped]) extends Actor with Stash {

  private implicit val executionContext = context.dispatcher

  override val supervisorStrategy = SupervisorStrategies.escalate
  private implicit val askTimeout = Timeout(99.seconds)
  private val journalActor = context.watch(context.actorOf(
    JournalActor.props(journalMeta, MyConfig, new StampedKeyedEventBus, Scheduler.global, journalStopped,
      new EventIdGenerator(new EventIdClock.Fixed(currentTimeMillis = 1000/*EventIds start at 1000000*/))),
    "Journal"))
  private val keyToAggregate = mutable.Map[String, ActorRef]()
  private var terminator: ActorRef = null

  override def preStart() = {
    super.preStart()
    val recoverer = new MyJournalRecoverer()
    recoverer.recoverAllAndTransferTo(journalActor = journalActor)
    keyToAggregate ++= recoverer.recoveredJournalingActors.keyToJournalingActor map {
      case (k: String, a) ⇒ k → a
      case o ⇒ sys.error(s"UNEXPECTED: $o")
    }
  }

  private class MyJournalRecoverer extends JournalActorRecoverer[TestEvent] {
    protected val sender = TestActor.this.sender()
    protected val journalMeta = TestActor.this.journalMeta
    protected val journalEventWatch = new JournalEventWatch[TestEvent](journalMeta, JournalEventWatch.TestConfig)(
      Scheduler.global, TimerService(Some(1.s)))

    protected def snapshotToKey = {
      case a: TestAggregate ⇒ a.key
    }

    protected def isDeletedEvent = Set(TestEvent.Removed)

    def recoverSnapshot = {
      case snapshot: TestAggregate ⇒
        recoverActorForSnapshot(snapshot, newAggregateActor(snapshot.key))
    }

    def recoverNewKey = {
      case stamped @ Stamped(_, _, KeyedEvent(key: String, _: TestEvent.Added)) ⇒
        recoverActorForNewKey(stamped, newAggregateActor(key))

      case _ ⇒
    }
  }

  def receive = {
    case JournalRecoverer.Output.JournalIsReady ⇒
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.WaitUntilReady ⇒
      sender() ! Done

    case Input.Forward(key: String, command: TestAggregateActor.Command.Add) ⇒
      assert(!keyToAggregate.contains(key))
      val actor = newAggregateActor(key)
      keyToAggregate += key → actor
      (actor ? command).mapTo[Done] pipeTo sender()

    case Input.Forward(key: String, command @ TestAggregateActor.Command.Remove) ⇒
      val aggregateActor = keyToAggregate(key)
      val respondTo = this.sender()
      (aggregateActor ? command).mapTo[TestAggregateActor.Response.Completed] foreach { response ⇒
        // Respond first when actor TestAggregateActor has been terminated
        context.actorOf(Props {
          new Actor {
            context.watch(aggregateActor)
            def receive = {
              case Terminated(`aggregateActor`) ⇒
                respondTo ! response
                context.stop(self)
            }
          }
        })
      }
      keyToAggregate -= key

    case Input.Forward(key: String, disturb: TestAggregateActor.Command.Disturb) ⇒
      keyToAggregate(key) ! disturb

    case Input.Forward(key: String, command: TestAggregateActor.Command.DisturbAndRespond.type) ⇒
      (keyToAggregate(key) ? command).mapTo[String] pipeTo sender()

    case Input.Forward(key: String, command: TestAggregateActor.Command) ⇒
      (keyToAggregate(key) ? command).mapTo[TestAggregateActor.Response.Completed] pipeTo sender()

    case Input.GetAll ⇒
      sender() ! (keyToAggregate.values map { a ⇒ (a ? TestAggregateActor.Input.Get).mapTo[TestAggregate] await 99.s }).toVector

    case Input.TakeSnapshot ⇒
      (journalActor ? JournalActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] pipeTo sender()

    case Input.GetJournalState ⇒
      journalActor.forward(JournalActor.Input.GetState)

    case Input.Terminate ⇒
      terminator = sender()
      if (keyToAggregate.isEmpty) {
        journalActor ! JournalActor.Input.AwaitAndTerminate
      } else {
        keyToAggregate.values foreach context.stop
      }

    case Terminated(`journalActor`) if terminator != null ⇒
      context.stop(self)
      terminator ! Done

    case Terminated(actorRef) ⇒
      val key = keyToAggregate collectFirst { case (k, `actorRef`) ⇒ k }
      keyToAggregate --= key
      if (terminator != null && keyToAggregate.isEmpty) {
        journalActor ! JournalActor.Input.AwaitAndTerminate
      }
  }

  private def newAggregateActor(key: String): ActorRef =
    context.watch(context.actorOf(
      Props { new TestAggregateActor(key, journalActor) },
      s"Test-$key"))
}

private[journal] object TestActor {
  intelliJuseImport(TestKeyedEventJsonCodec)

  private val MyConfig = ConfigFactory.parseString("""
     |jobscheduler.journal.sync = on
     |jobscheduler.journal.delay = 0.s
     |jobscheduler.journal.simulate-sync = 10ms
     |jobscheduler.journal.snapshot.log-period = 10.ms
     |jobscheduler.journal.snapshot.log-actor-limit = 1
     |""".stripMargin)
  private val logger = Logger(getClass)

  object Input {
    final case object WaitUntilReady
    final case object TakeSnapshot
    final case class Forward(key: String, command: TestAggregateActor.Command)
    final case object GetJournalState
    final case object GetAll
    final case object Terminate
  }
}
