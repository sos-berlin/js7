package js7.tests

import cats.syntax.traverse.*
import js7.agent.RunningAgent
import js7.base.configutils.Configs.*
import js7.base.crypt.silly.{SillySignature, SillySignatureVerifier, SillySigner}
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.{autoClosing, multipleAutoClosing}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerEvent
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{VersionId, VersionedEvent}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.RecoveryTest.*
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.{StdoutOutput, script, toLocalSubagentId}
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class RecoveryTest extends OurTestSuite
{
  // TODO Starte Controller und Agenten in eigenen Prozessen, die wir abbrechen können.

  "test" in {
    for (_ <- if (sys.props contains "test.infinite") Iterator.from(1) else Iterator(1)) {
      val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), TestWorkflow.path)
      val Seq(order1, order2, order3) = orders
      var lastEventId = EventId.BeforeFirst
      val directoryProvider = new DirectoryProvider(
        AgentPaths,
        bareSubagents = Map.empty,
        TestWorkflow :: QuickWorkflow :: Nil,
        signer = new SillySigner(SillySignature("MY-SILLY-SIGNATURE")),
        verifier = new SillySignatureVerifier(SillySignature("MY-SILLY-SIGNATURE") :: Nil, "RecoveryTest"),
        testName = Some("RecoveryTest"),
        controllerConfig = TestConfig)
      autoClosing(directoryProvider) { _ =>
        for (agent <- directoryProvider.agentToTree.values)
          agent.writeExecutable(TestPathExecutable, script(1.s, resultVariable = Some("var1")))

        runController(directoryProvider) { controller =>
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = controller.eventWatch.tornEventId
          }
          controller.eventWatch.await[ControllerEvent.ControllerReady](after = lastEventId)
          import directoryProvider.sign
          assert(controller.eventWatch.await[VersionedEvent]().map(_.value).sortBy(_.toString) ==
            Vector(
              NoKey <-: VersionAdded(VersionId("INITIAL")),
              NoKey <-: VersionedItemAdded(sign(TestWorkflow)),
              NoKey <-: VersionedItemAdded(sign(QuickWorkflow)))
            .sortBy(_.toString))
          runAgents(directoryProvider) { _ =>
            controller.addOrderBlocking(order1)
            controller.addOrderBlocking(QuickOrder)
            /*lastEventId =*/ lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == QuickOrder.id))
            lastEventId = lastEventIdOf(controller.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
            controller.addOrderBlocking(order2)
            lastEventId = lastEventIdOf(controller.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
          }

          logger.info("*** RESTARTING AGENTS ***\n")
          runAgents(directoryProvider) { _ =>
            lastEventId = lastEventIdOf(controller.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
            controller.addOrderBlocking(order3)
          }
        }

        for (i <- 1 to 2) withClue(s"Run #$i:") {
          val myLastEventId = lastEventId
          //sys.runtime.gc()  // For a clean memory view
          logger.info(s"*** RESTARTING CONTROLLER AND AGENTS #$i ***\n")
          runAgents(directoryProvider) { _ =>
            runController(directoryProvider) { controller =>
              val orderId = controller.eventWatch.await[OrderFinished](after = myLastEventId, predicate = _.key == orders(i).id).last.value.key
              val orderStampeds = controller.eventWatch.await[Event](_.key == orderId)
              withClue(s"$orderId") {
                try assert(deleteRestartedJobEvents(orderStampeds.map(_.value.event).iterator).toVector == ExpectedOrderEvents)
                catch { case NonFatal(t) =>
                  logger.error("Test failed due to unexpected events:\n" + orderStampeds.mkString("\n"))
                  throw t
                }
                assert(controller.controllerState.await(99.s).idToOrder.keySet == (orders :+ QuickOrder).map(_.id).toSet)
              }
            }
          }
        }
      }
    }
  }

  private def runController(directoryProvider: DirectoryProvider)(body: RunningController => Unit): Unit =
    directoryProvider.runController() { controller =>
      // TODO Duplicate code in DirectoryProvider
      try body(controller)
      catch { case NonFatal(t) =>
        logger.error(t.toStringWithCauses) /* Pekko may crash before the caller gets the error so we log the error here */
        try controller.terminate(suppressSnapshot = true) await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
      logger.info("🔥🔥🔥 TERMINATE CONTROLLER 🔥🔥🔥")
      // Kill Controller ActorSystem
      Pekkos.terminateAndWait(controller.actorSystem, 99.s)
    }

  private def runAgents(directoryProvider: DirectoryProvider)(body: IndexedSeq[RunningAgent] => Unit): Unit = {
    val agents = directoryProvider.agents
      .map(_.agentConfiguration)
      .traverse(RunningAgent.startForTest(_))
      .await(10.s)
    multipleAutoClosing(agents) { _ =>
      // TODO Duplicate code in DirectoryProvider
      try body(agents)
      catch { case NonFatal(t) =>
        logger.error(t.toStringWithCauses) /* Pekko may crash before the caller gets the error so we log the error here */
        try agents.traverse(_.terminate()) await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
      // Kill Agents ActorSystems
      for (agent <- agents) Pekkos.terminateAndWait(agent.actorSystem, 99.s)
    }
  }
}

private object RecoveryTest {
  private val logger = Logger(getClass)

  private val TestConfig = config"js7.journal.remove-obsolete-files = false"
  private val AgentPaths = AgentPath("agent-111") :: AgentPath("agent-222") :: Nil
  private val subagentIds = AgentPaths.map(toLocalSubagentId)
  private val TestPathExecutable = RelativePathExecutable("TEST.cmd", v1Compatible = true)

  private val TestWorkflow = Workflow(WorkflowPath("test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-1")),
      Execute(WorkflowJob.Name("TEST-1"))),
    Map(
      WorkflowJob.Name("TEST-0") -> WorkflowJob(AgentPaths(0), TestPathExecutable, Map("var1" -> StringConstant(s"VALUE-${AgentPaths(0).string}"))),
      WorkflowJob.Name("TEST-1") -> WorkflowJob(AgentPaths(1), TestPathExecutable, Map("var1" -> StringConstant(s"VALUE-${AgentPaths(1).string}")))))

  private val QuickWorkflow = Workflow.of(WorkflowPath("quick") ~ "INITIAL", Execute(WorkflowJob(AgentPaths(0), TestPathExecutable)))
  private val QuickOrder = FreshOrder(OrderId("QUICK-ORDER"), QuickWorkflow.id.path)

  private val ExpectedOrderEvents = Vector(
    OrderAdded(TestWorkflow.id),
    OrderAttachable(AgentPaths(0)),
    OrderAttached(AgentPaths(0)),
    OrderStarted,
    OrderProcessingStarted(subagentIds(0)),
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(1)),
    OrderProcessingStarted(subagentIds(0)),
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(2)),
    OrderProcessingStarted(subagentIds(0)),
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderDetached,
    OrderAttachable(AgentPaths(1)),
    OrderAttached(AgentPaths(1)),
    OrderProcessingStarted(subagentIds(1)),
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-222")))),
    OrderMoved(Position(4)),
    OrderProcessingStarted(subagentIds(1)),
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-222")))),
    OrderMoved(Position(5)),
    OrderDetachable,
    OrderDetached,
    OrderFinished())

  /** Deletes restart sequences to make event sequence comparable with ExpectedOrderEvents. */
  private def deleteRestartedJobEvents(events: Iterator[Event]): Seq[Event] = {
    val result = mutable.Buffer[Event]()
    while (events.hasNext) {
      events.next() match {
        case OrderProcessed.processLostDueToRestart =>
          while (!result.last.isInstanceOf[OrderProcessingStarted]) {
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

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId = {
    val last = stamped.iterator.to(Vector).last
    logger.debug(s"lastEventIdOf => $last")
    last.eventId
  }
}
