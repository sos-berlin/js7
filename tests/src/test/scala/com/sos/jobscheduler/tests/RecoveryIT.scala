package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentEvent
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichEither, RichThrowable}
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
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.Order.Scheduled
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderMovedToAgent, OrderMovedToMaster, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JournalMeta, JsonFileIterator}
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.RecoveryIT._
import java.nio.file.Path
import java.time.Instant
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
        import directoryProvider.directory

        (directoryProvider.master.live / "fast.job_chain.xml").xml = QuickJobChainElem
        (directoryProvider.master.live / "test.job_chain.xml").xml = TestJobChainElem
        (directoryProvider.master.live / "test.order.xml").xml = TestOrderGeneratorElem

        val agentConfs = directoryProvider.agents map (_.conf)

        runMaster(directory) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = eventCollector.oldestEventId
          }
          runAgents(agentConfs) { _ ⇒
            master.executeCommand(MasterCommand.AddOrderIfNew(FastOrder)) await 99.s
            lastEventId = lastEventIdOf(eventCollector.when[OrderFinished.type](EventRequest.singleClass(after = lastEventId, 99.s), _.key == FastOrderId) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderProcessed](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestWorkflowPath.string) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderProcessed](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestWorkflowPath.string) await 99.s)
          }
          assert((readEvents(directoryProvider.agents(0).data / "state/journal") map { case Stamped(_, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous)))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(agentConfs) { _ ⇒
            lastEventId = lastEventIdOf(eventCollector.when[OrderProcessed](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestWorkflowPath.string) await 99.s)
          }
        }

        for (i ← 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(agentConfs) { _ ⇒
            runMaster(directory) { master ⇒
              val finishedEventSeq = eventCollector.when[OrderFinished.type](EventRequest.singleClass(after = myLastEventId, 99.s), _.key.string startsWith TestWorkflowPath.string) await 99.s
              val orderId = (finishedEventSeq: @unchecked) match {
                case eventSeq: EventSeq.NonEmpty[Iterator, KeyedEvent[OrderFinished.type]] ⇒ eventSeq.stampeds.toVector.last.value.key
              }
              assert(master.orderClient.order(orderId).await(99.s) ==
                Some(Order(
                  orderId,
                  NodeKey(TestWorkflowPath, NodeId("END")),
                  Order.Finished,
                  payload = Payload(
                    Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")))))
              val EventSeq.NonEmpty(eventSeq) = eventCollector.when[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, 0.s), _.key == orderId) await 99.s
              withClue(s"$orderId") {
                assert((deleteRestartedJobEvents(eventSeq map { _.value.event }) collect {
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

  private def runMaster(directory: Path)(body: RunningMaster ⇒ Unit): Unit =
    RunningMaster.runForTest(directory) { master ⇒
      eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
      master.executeCommand(MasterCommand.ScheduleOrdersEvery(2.s.toFiniteDuration))  // Will block on recovery until Agents are started: await 99.s
      body(master)
      logger.info("🔥🔥🔥 TERMINATE MASTER 🔥🔥🔥")
    }

  private def runAgents(confs: Seq[AgentConfiguration])(body: Seq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(confs map startAgent await 10.s) { agents ⇒
      body(agents)
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
    }

  private def startAgent(conf: AgentConfiguration): Future[RunningAgent] = {
    val whenAgent = RunningAgent(conf)
    for (agent ← whenAgent; t ← agent.terminated.failed) logger.error(t.toStringWithCauses, t)
    whenAgent
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
  private val FastOrder = Order(FastOrderId, NodeKey(FastWorkflowPath, NodeId("100")), Order.StartNow)
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
    <order job_chain={TestWorkflowPath.string} state="100">
      <run_time><period absolute_repeat="3"/></run_time>
    </order>

  private val QuickJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="agent-111" job="/test"/>
      <job_chain_node.end state="END"/>
    </job_chain>

  private val ExpectedEvents = Vector(
    OrderAdded(NodeKey(TestWorkflowPath, NodeId("100")), Scheduled(SomeTimestamp), Payload(Map())),
    OrderMovedToAgent(AgentPaths(0)),
    OrderProcessingStarted,
    OrderStdoutWritten(s"$StdoutOutput\n"),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-111")), Outcome.Good(true)),
    OrderMoved(NodeId("110")),
    OrderProcessingStarted,
    OrderStdoutWritten(s"$StdoutOutput\n"),
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(NodeId("120")),
    OrderProcessingStarted,
    OrderStdoutWritten(s"$StdoutOutput\n"),
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderDetachable,
    OrderMovedToMaster,
    OrderMoved(NodeId("200")),
    OrderMovedToAgent(AgentPaths(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(s"$StdoutOutput\n"),
    OrderProcessed(MapDiff(Map("result" → "SCRIPT-VARIABLE-VALUE-agent-222")), Outcome.Good(true)),
    OrderMoved(NodeId("210")),
    OrderProcessingStarted,
    OrderStdoutWritten(s"$StdoutOutput\n"),
    OrderProcessed(MapDiff(Map(), Set()), Outcome.Good(true)),
    OrderDetachable,
    OrderMovedToMaster,
    OrderMoved(NodeId("END")),
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

  private def lastEventIdOf[E <: Event](eventSeq: TearableEventSeq[Iterator, KeyedEvent[E]]): EventId =
    (eventSeq: @unchecked) match {
      case eventSeq: EventSeq.NonEmpty[Iterator, KeyedEvent[E]] ⇒
        eventSeq.stampeds.toVector.last.eventId
    }
}
