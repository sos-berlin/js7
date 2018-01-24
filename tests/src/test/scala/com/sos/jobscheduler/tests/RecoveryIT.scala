package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.scheduler.AgentEvent
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.Order.Scheduled
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.{JobPath, Position, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JournalMeta, JsonFileIterator}
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.RecoveryIT._
import java.nio.file.Path
import java.time.Instant
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class RecoveryIT extends FreeSpec {

  private val eventCollector = new TestEventCollector

  "test" in {
    var lastEventId = EventId.BeforeFirst
    autoClosing(new DirectoryProvider(AgentPaths)) { directoryProvider ⇒
      for ((agentPath, tree) ← directoryProvider.agentToTree)
        tree.job(TestJobPath).xml = jobXml(1.s, Map("var1" → s"VALUE-${agentPath.name}"), resultVariable = Some("var1"))
      withCloser { implicit closer ⇒

        (directoryProvider.master.live / "fast.job_chain.xml").xml = QuickJobChainElem
        (directoryProvider.master.live / "test.job_chain.xml").xml = TestJobChainElem
        (directoryProvider.master.live / "test.order.xml").xml = TestOrderGeneratorElem

        runMaster(directoryProvider) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = eventCollector.oldestEventId
          }
          runAgents(directoryProvider) { _ ⇒
            master.addOrder(FastOrder) await 99.s
            lastEventId = lastEventIdOf(eventCollector.await[OrderFinished.type](after = lastEventId, predicate = _.key == FastOrderId))
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflowPath.string))
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflowPath.string))
          }
          assert((readEvents(directoryProvider.agents(0).data / "state/journal") map { case Stamped(_, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous)))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(directoryProvider) { _ ⇒
            lastEventId = lastEventIdOf(eventCollector.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflowPath.string))
          }
        }

        for (i ← 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(directoryProvider) { _ ⇒
            runMaster(directoryProvider) { master ⇒
              val orderId = eventCollector.await[OrderFinished.type](after = myLastEventId, predicate = _.key.string startsWith TestWorkflowPath.string).last.value.key
              assert(master.orderClient.order(orderId).await(99.s) ==
                Some(Order(
                  orderId,
                  TestWorkflowPath /: Position(5),
                  Order.Finished,
                  payload = Payload(
                    Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")))))
              val stampeds = eventCollector.await[OrderEvent](_.key == orderId)
              withClue(s"$orderId") {
                assert((deleteRestartedJobEvents(stampeds.map(_.value.event).iterator) collect {
                    case o @ OrderAdded(_, Order.Scheduled(_), _) ⇒ o.copy(state = Order.Scheduled(SomeTimestamp))
                    case o ⇒ o
                  }).toVector
                  == ExpectedEvents)
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

private object RecoveryIT {
  private val logger = Logger(getClass)

  private val AgentPaths = List(AgentPath("/agent-111"), AgentPath("/agent-222"))
  private val TestWorkflowPath = WorkflowPath("/test")
  private val FastWorkflowPath = WorkflowPath("/fast")
  private val TestJobPath = JobPath("/test")

  private val FastOrderId = OrderId("FAST-ORDER")
  private val FastOrder = Order(FastOrderId, FastWorkflowPath, Order.StartNow)
  private val SomeTimestamp = Instant.parse("2017-07-23T12:00:00Z").toTimestamp

  private val TestJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="agent-111" job="/test"/>
      <job_chain_node state="110" agent="agent-111" job="/test"/>
      <job_chain_node state="120" agent="agent-111" job="/test"/>
      <job_chain_node state="200" agent="agent-222" job="/test"/>
      <job_chain_node state="210" agent="agent-222" job="/test"/>
      <job_chain_node.end state="END"/>
    </job_chain>

  private val TestOrderGeneratorElem =
    <order job_chain={TestWorkflowPath.string}>
      <run_time><period absolute_repeat="3"/></run_time>
    </order>

  private val QuickJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="agent-111" job="/test"/>
      <job_chain_node.end state="END"/>
    </job_chain>

  private val ExpectedEvents = Vector(
    OrderAdded(TestWorkflowPath, Scheduled(SomeTimestamp), Payload(Map())),
    OrderTransferredToAgent(AgentPaths(0)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-111")), Outcome.Good(true)),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderTransferredToAgent(AgentPaths(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")), Outcome.Good(true)),
    OrderMoved(Position(4)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(MapDiff(Map(), Set()), Outcome.Good(true)),
    OrderMoved(Position(5)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  /** Deletes restart sequences to make event sequence comparable with ExpectedEvents. */
  private def deleteRestartedJobEvents(events: Iterator[OrderEvent]): Seq[OrderEvent] = {
    val result = mutable.Buffer[OrderEvent]()
    while (events.hasNext) {
      events.next() match {
        case OrderProcessed(_, Outcome.Bad(Outcome.Bad.AgentRestarted)) ⇒
          while (result.last != OrderEvent.OrderProcessingStarted) {
            result.remove(result.size - 1)
          }
          result.remove(result.size - 1)
          assert(events.next.isInstanceOf[OrderEvent.OrderMoved])  // Not if Agent restarted immediately after recovery (not expected)

        case event ⇒ result += event
      }
    }
    result.toVector
  }

  private def lastEventIdOf[E <: Event](stamped: TraversableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.toVector.last.eventId
}
