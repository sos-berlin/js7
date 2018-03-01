package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.scheduler.AgentEvent
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.{GzipCompression, JournalMeta, JsonFileIterator}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBasedVersion, RepoEvent, SourceType}
import com.sos.jobscheduler.data.order.Order.Scheduled
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{JobPath, Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.master.order.agent.Agent
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.RecoveryTest._
import java.nio.file.Path
import java.time.Instant
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class RecoveryTest extends FreeSpec {

  private val eventCollector = new TestEventCollector

  "test" in {
    var lastEventId = EventId.BeforeFirst
    autoClosing(new DirectoryProvider(AgentPaths)) { directoryProvider ⇒
      for ((agentPath, tree) ← directoryProvider.agentToTree)
        tree.file(TestJobPath, SourceType.Xml).xml = jobXml(1.s, Map("var1" → s"VALUE-${agentPath.name}"), resultVariable = Some("var1"))
      withCloser { implicit closer ⇒
        for (w ← Array(TestNamedWorkflow, QuickNamedWorkflow)) directoryProvider.master.writeJson(w.path, w.workflow)
        (directoryProvider.master.live / "test.order.xml").xml = TestOrderGeneratorElem

        runMaster(directoryProvider) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = eventCollector.oldestEventId
          }
          eventCollector.await[MasterEvent.MasterReady](after = lastEventId)
          assert(eventCollector.await[RepoEvent](_ ⇒ true).map(_.value).sortBy(_.toString) == Vector(
            NoKey <-: VersionAdded(FileBasedVersion("(INITIAL)")),
            NoKey <-: FileBasedAdded(TestNamedWorkflow),
            NoKey <-: FileBasedAdded(QuickNamedWorkflow),
            NoKey <-: FileBasedAdded(Agent(AgentPaths(0), s"http://127.0.0.1:${directoryProvider.agents(0).conf.http.get.address.getPort}")),
            NoKey <-: FileBasedAdded(Agent(AgentPaths(1), s"http://127.0.0.1:${directoryProvider.agents(1).conf.http.get.address.getPort}"))).sortBy(_.toString))
          runAgents(directoryProvider) { _ ⇒
            master.addOrder(QuickOrder) await 99.s
            lastEventId = lastEventIdOf(eventCollector.await[OrderFinished](after = lastEventId, predicate = _.key == QuickOrder.id))
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestNamedWorkflow.path.string))
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestNamedWorkflow.path.string))
          }
          assert((readEvents(directoryProvider.agents(0).data / "state/journal") map { case Stamped(_, _, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous)))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(directoryProvider) { _ ⇒
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestNamedWorkflow.path.string))
          }
        }

        for (i ← 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(directoryProvider) { _ ⇒
            runMaster(directoryProvider) { master ⇒
              val orderId = eventCollector.await[OrderFinished](after = myLastEventId, predicate = _.key.string startsWith TestNamedWorkflow.path.string).last.value.key
              assert(master.orderClient.order(orderId).await(99.s) ==
                Some(Order(
                  orderId,
                  TestNamedWorkflow.path /: Position(5),
                  Order.Finished,
                  payload = Payload(
                    Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")))))

              val orderStampeds = eventCollector.await[Event](_.key == orderId)
              withClue(s"$orderId") {
                try assert((deleteRestartedJobEvents(orderStampeds.map(_.value.event).iterator) collect {
                    case o @ OrderAdded(_, Order.Scheduled(_), _) ⇒ o.copy(state = Order.Scheduled(SomeTimestamp))
                    case o ⇒ o
                  }).toVector
                  == ExpectedOrderEvents)
                catch { case NonFatal(t) ⇒
                  logger.error("Test failed due to unexpected events:\n" + orderStampeds.mkString("\n"))
                  throw t
                }
              }
            }
          }
        }
      }
    }
  }

  private def runMaster(directoryProvider: DirectoryProvider)(body: RunningMaster ⇒ Unit): Unit =
    RunningMaster.runForTest(directoryProvider.directory) { master ⇒
      eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
      master.executeCommand(MasterCommand.ScheduleOrdersEvery(2.s.toFiniteDuration))  // Will block on recovery until Agents are started: await 99.s
      body(master)
      logger.info("🔥🔥🔥 TERMINATE MASTER 🔥🔥🔥")
    }

  private def runAgents(directoryProvider: DirectoryProvider)(body: IndexedSeq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(directoryProvider.agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
    }

  private def readEvents(journalFile: Path): Vector[Stamped[KeyedEvent[AgentEvent]]] = {
    val conversion = new GzipCompression {}
    autoClosing(new JsonFileIterator(JournalMeta.Header, in ⇒ conversion.convertInputStream(in, journalFile), journalFile)) {
      _.toVector collect {
        case o if AgentEvent.KeyedEventJsonCodec.canDeserialize(o) ⇒
          o.as[Stamped[KeyedEvent[AgentEvent]]].force
      }
    }
  }
}

private object RecoveryTest {
  private val logger = Logger(getClass)

  private val AgentPaths = List(AgentPath("/agent-111"), AgentPath("/agent-222"))
  private val TestJobPath = JobPath("/test")

  private val SomeTimestamp = Instant.parse("2017-07-23T12:00:00Z").toTimestamp

  private val TestNamedWorkflow = Workflow.Named(
    WorkflowPath("/test"),
    Workflow.of(
      Job(TestJobPath, AgentPaths(0)),
      Job(TestJobPath, AgentPaths(0)),
      Job(TestJobPath, AgentPaths(0)),
      Job(TestJobPath, AgentPaths(1)),
      Job(TestJobPath, AgentPaths(1))))
  private val TestOrderGeneratorElem =
    <order job_chain={TestNamedWorkflow.path.string}>
      <run_time><period absolute_repeat="3"/></run_time>
    </order>

  private val QuickNamedWorkflow = Workflow.Named(WorkflowPath("/quick"), Workflow.of(Job(TestJobPath, AgentPaths(0))))
  private val QuickOrder = Order(OrderId("FAST-ORDER"), QuickNamedWorkflow.path, Order.StartNow)

  private val ExpectedOrderEvents = Vector(
    OrderAdded(TestNamedWorkflow.path, Scheduled(SomeTimestamp), Payload(Map())),
    OrderTransferredToAgent(AgentPaths(0)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-111")), Outcome.succeeded),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderTransferredToAgent(AgentPaths(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")), Outcome.succeeded),
    OrderMoved(Position(4)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map(), Set()), Outcome.succeeded),
    OrderMoved(Position(5)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  /** Deletes restart sequences to make event sequence comparable with ExpectedOrderEvents. */
  private def deleteRestartedJobEvents(events: Iterator[Event]): Seq[Event] = {
    val result = mutable.Buffer[Event]()
    while (events.hasNext) {
      events.next() match {
        case OrderProcessed(_, Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted)) ⇒
          while (result.last != OrderEvent.OrderProcessingStarted) {
            result.remove(result.size - 1)
          }
          result.remove(result.size - 1)
          val e = events.next()
          assert(e.isInstanceOf[OrderEvent.OrderMoved])  // Not if Agent restarted immediately after recovery (not expected)

        case event ⇒ result += event
      }
    }
    result.toVector
  }

  private def lastEventIdOf[E <: Event](stamped: TraversableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.toVector.last.eventId
}
