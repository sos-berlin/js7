package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.scheduler.AgentEvent
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{<-:, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, VersionAdded}
import com.sos.jobscheduler.data.filebased.{RepoEvent, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.RecoveryTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import java.nio.file.Path
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
    for (_ <- if (sys.props contains "test.infinite") Iterator.from(1) else Iterator(1)) {
      val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), TestWorkflow.path)
      val Seq(order1, order2, order3) = orders
      var lastEventId = EventId.BeforeFirst
      val directoryProvider = new DirectoryProvider(
        AgentRefPaths,
        TestWorkflow :: QuickWorkflow :: Nil,
        signer = new SillySigner(SillySignature("MY-SILLY-SIGNATURE")),
        testName = Some("RecoveryTest"))
      autoClosing(directoryProvider) { _ =>
        for (agent <- directoryProvider.agentToTree.values)
          agent.writeExecutable(TestExecutablePath, script(1.s, resultVariable = Some("var1")))

        runMaster(directoryProvider) { master =>
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = master.eventWatch.tornEventId
          }
          master.eventWatch.await[MasterEvent.MasterReady](after = lastEventId)
          import directoryProvider.sign
          assert(master.eventWatch.await[RepoEvent]().map(_.value).sortBy(_.toString) ==
            Vector(
              NoKey <-: VersionAdded(VersionId("INITIAL")),
              NoKey <-: FileBasedAdded(AgentRefPaths(0), sign(AgentRef(AgentRefPaths(0) ~ "INITIAL", directoryProvider.agents(0).localUri.toString))),
              NoKey <-: FileBasedAdded(AgentRefPaths(1), sign(AgentRef(AgentRefPaths(1) ~ "INITIAL", directoryProvider.agents(1).localUri.toString))),
              NoKey <-: FileBasedAdded(TestWorkflow.path, sign(TestWorkflow)),
              NoKey <-: FileBasedAdded(QuickWorkflow.path, sign(QuickWorkflow)))
            .sortBy(_.toString))
          runAgents(directoryProvider) { _ =>
            master.addOrderBlocking(order1)
            master.addOrderBlocking(QuickOrder)
            /*lastEventId =*/ lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == QuickOrder.id))
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
            master.addOrderBlocking(order2)
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
          }
          val Vector(Stamped(_, _, NoKey <-: AgentEvent.MasterRegistered(MasterId("Master")/*see default master.conf*/, _/*agentRunId*/))) =
            readAgentEvents(directoryProvider.agents(0).dataDir / "state/agent--0.journal")

          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(directoryProvider) { _ =>
            lastEventId = lastEventIdOf(master.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
            master.addOrderBlocking(order3)
          }
        }

        for (i <- 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          //sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(directoryProvider) { _ =>
            runMaster(directoryProvider) { master =>
              val orderId = master.eventWatch.await[OrderFinished](after = myLastEventId, predicate = _.key == orders(i).id).last.value.key
              val orderStampeds = master.eventWatch.await[Event](_.key == orderId)
              withClue(s"$orderId") {
                try assert(deleteRestartedJobEvents(orderStampeds.map(_.value.event).iterator).toVector == ExpectedOrderEvents)
                catch { case NonFatal(t) =>
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

  private def runMaster(directoryProvider: DirectoryProvider)(body: RunningMaster => Unit): Unit =
    directoryProvider.runMaster() { master =>
      body(master)
      logger.info("🔥🔥🔥 TERMINATE MASTER 🔥🔥🔥")
      // Kill Master ActorSystem
      master.actorSystem.terminate() await 99.s
    }

  private def runAgents(directoryProvider: DirectoryProvider)(body: IndexedSeq[RunningAgent] => Unit): Unit =
    multipleAutoClosing(directoryProvider.agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents =>
      body(agents)
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
      // Kill Agents ActorSystems
      for (agent <- agents) agent.actorSystem.terminate() await 99.s
    }

  private def readAgentEvents(journalFile: Path): Vector[Stamped[KeyedEvent[AgentEvent]]] =
    autoClosing(InputStreamJsonSeqReader.open(journalFile)) { reader =>
      UntilNoneIterator(reader.read).toVector
        .map(_.value)
        .collect {
          case json if AgentEvent.KeyedEventJsonCodec.canDeserialize(json) =>
            import AgentEvent.KeyedEventJsonCodec
            json.as[Stamped[KeyedEvent[Event]]].orThrow
        }
        .collect {
          case o @ Stamped(_, _, KeyedEvent(_, _: AgentEvent)) => o.asInstanceOf[Stamped[KeyedEvent[AgentEvent]]]  // Ignore SnapshotTaken
        }
    }
}

private object RecoveryTest {
  private val logger = Logger(getClass)

  private val AgentRefPaths = AgentRefPath("/agent-111") :: AgentRefPath("/agent-222") :: Nil
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-1")),
      Execute(WorkflowJob.Name("TEST-1"))),
    Map(
      WorkflowJob.Name("TEST-0") -> WorkflowJob(AgentRefPaths(0), TestExecutablePath, Map("var1" -> s"VALUE-${AgentRefPaths(0).name}")),
      WorkflowJob.Name("TEST-1") -> WorkflowJob(AgentRefPaths(1), TestExecutablePath, Map("var1" -> s"VALUE-${AgentRefPaths(1).name}"))))

  private val QuickWorkflow = Workflow.of(WorkflowPath("/quick") ~ "INITIAL", Execute(WorkflowJob(AgentRefPaths(0), TestExecutablePath)))
  private val QuickOrder = FreshOrder(OrderId("QUICK-ORDER"), QuickWorkflow.id.path)

  private val ExpectedOrderEvents = Vector(
    OrderAdded(TestWorkflow.id, None, Map.empty),
    OrderAttachable(AgentRefPaths(0)),
    OrderTransferredToAgent(AgentRefPaths(0)),
    OrderStarted,
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("result" -> "SCRIPT-VARIABLE-VALUE-agent-111"))),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("result" -> "SCRIPT-VARIABLE-VALUE-agent-111"))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("result" -> "SCRIPT-VARIABLE-VALUE-agent-111"))),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderAttachable(AgentRefPaths(1)),
    OrderTransferredToAgent(AgentRefPaths(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("result" -> "SCRIPT-VARIABLE-VALUE-agent-222"))),
    OrderMoved(Position(4)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("result" -> "SCRIPT-VARIABLE-VALUE-agent-222"))),
    OrderMoved(Position(5)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  /** Deletes restart sequences to make event sequence comparable with ExpectedOrderEvents. */
  private def deleteRestartedJobEvents(events: Iterator[Event]): Seq[Event] = {
    val result = mutable.Buffer[Event]()
    while (events.hasNext) {
      events.next() match {
        case OrderProcessed(Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted)) =>
          while (result.last != OrderEvent.OrderProcessingStarted) {
            result.remove(result.size - 1)
          }
          result.remove(result.size - 1)
          val e = events.next()
          assert(e.isInstanceOf[OrderEvent.OrderMoved])  // Not if Agent restarted immediately after recovery (not expected)

        case event => result += event
      }
    }
    result.toVector
  }

  private def lastEventIdOf[E <: Event](stamped: TraversableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.toVector.last.eventId
}
