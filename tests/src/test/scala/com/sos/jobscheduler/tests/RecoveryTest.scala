package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.scheduler.AgentEvent
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, VersionAdded}
import com.sos.jobscheduler.data.filebased.{RepoEvent, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.RecoveryTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import java.nio.file.Path
import java.time.Instant
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class RecoveryTest extends FreeSpec {

  // TODO Starte Master und Agenten in eigenen Prozessen, die wir abbrechen können.

  "test" in {
    for (_ ← if (sys.props contains "test.infinite") Iterator.from(1) else Iterator(1)) {
      var lastEventId = EventId.BeforeFirst
      autoClosing(new DirectoryProvider(AgentIds map (_.path), TestWorkflow :: QuickWorkflow :: Nil, testName = Some("RecoveryTest"))) {
        directoryProvider ⇒
        for (agent ← directoryProvider.agentToTree.values)
          agent.writeExecutable(TestExecutablePath, script(1.s, resultVariable = Some("var1")))
        (directoryProvider.master.orderGenerators / "test.order.xml").xml = TestOrderGeneratorElem

        runMaster(directoryProvider) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = master.eventWatch.tornEventId
          }
          master.eventWatch.await[MasterEvent.MasterReady](after = lastEventId)
          assert(master.eventWatch.await[RepoEvent]().map(_.value).sortBy(_.toString) ==
            Vector(
              NoKey <-: VersionAdded(VersionId("INITIAL")),
              NoKey <-: FileBasedAdded(Agent(AgentIds(0).path, directoryProvider.agents(0).localUri.toString)),
              NoKey <-: FileBasedAdded(Agent(AgentIds(1).path, directoryProvider.agents(1).localUri.toString)),
              NoKey <-: FileBasedAdded(TestWorkflow.withoutVersion),
              NoKey <-: FileBasedAdded(QuickWorkflow.withoutVersion))
            .sortBy(_.toString))
          runAgents(directoryProvider) { _ ⇒
            master.addOrderBlocking(QuickOrder)
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == QuickOrder.id))
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflow.path.string))
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflow.path.string))
          }
          assert((readEvents(directoryProvider.agents(0).data / "state/agent--0.journal") map { case Stamped(_, _, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded(MasterId("Master")/*see default master.conf*/))))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(directoryProvider) { _ ⇒
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key.string startsWith TestWorkflow.path.string))
          }
        }

        for (i ← 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(directoryProvider) { _ ⇒
            runMaster(directoryProvider) { master ⇒
              val orderId = master.eventWatch.await[OrderFinished](after = myLastEventId, predicate = _.key.string startsWith TestWorkflow.path.string).last.value.key
              val orderStampeds = master.eventWatch.await[Event](_.key == orderId)
              withClue(s"$orderId") {
                try assert((deleteRestartedJobEvents(orderStampeds.map(_.value.event).iterator) collect {
                    case o @ OrderAdded(_, Some(_), _) ⇒ o.copy(scheduledFor = Some(SomeTimestamp))
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
    directoryProvider.runMaster() { master ⇒
      master.executeCommandAsSystemUser(MasterCommand.ScheduleOrdersEvery(2.s.toFiniteDuration)).await(99.s).orThrow
      body(master)
      logger.info("🔥🔥🔥 TERMINATE MASTER 🔥🔥🔥")
      // Kill Master ActorSystem
      master.injector.instance[ActorSystem].terminate() await 99.s
    }

  private def runAgents(directoryProvider: DirectoryProvider)(body: IndexedSeq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(directoryProvider.agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
      // Kill Agents ActorSystems
      for (agent ← agents) agent.injector.instance[ActorSystem].terminate() await 99.s
    }

  private def readEvents(journalFile: Path): Vector[Stamped[KeyedEvent[AgentEvent]]] =
    autoClosing(InputStreamJsonSeqReader.open(journalFile)) { reader ⇒
      UntilNoneIterator(reader.read).toVector map (_.value) collect {
        case json if AgentEvent.KeyedEventJsonCodec.canDeserialize(json) ⇒
          json.as[Stamped[KeyedEvent[AgentEvent]]].orThrow
      }
    }
}

private object RecoveryTest {
  private val logger = Logger(getClass)

  private val AgentIds = List(AgentPath("/agent-111"), AgentPath("/agent-222")) map (_ % "INITIAL")
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val SomeTimestamp = Instant.parse("2017-07-23T12:00:00Z").toTimestamp

  private val TestWorkflow = Workflow(WorkflowPath("/test") % "INITIAL",
    Vector(
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-1")),
      Execute(WorkflowJob.Name("TEST-1"))),
    Map(
      WorkflowJob.Name("TEST-0") → WorkflowJob(AgentIds(0).path, TestExecutablePath, Map("var1" → s"VALUE-${AgentIds(0).path.name}")),
      WorkflowJob.Name("TEST-1") → WorkflowJob(AgentIds(1).path, TestExecutablePath, Map("var1" → s"VALUE-${AgentIds(1).path.name}"))))
  private val TestOrderGeneratorElem =
    <order job_chain={TestWorkflow.path.string}>
      <run_time><period absolute_repeat="3"/></run_time>
    </order>

  private val QuickWorkflow = Workflow.of(WorkflowPath("/quick") % "INITIAL", Execute(WorkflowJob(AgentIds(0).path, TestExecutablePath)))
  private val QuickOrder = FreshOrder(OrderId("FAST-ORDER"), QuickWorkflow.id.path)

  private val ExpectedOrderEvents = Vector(
    OrderAdded(TestWorkflow.id, Some(SomeTimestamp), Payload(Map())),
    OrderAttachable(AgentIds(0).path),
    OrderTransferredToAgent(AgentIds(0)),
    OrderStarted,
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
    OrderAttachable(AgentIds(1).path),
    OrderTransferredToAgent(AgentIds(1)),
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
