package js7.tests

import java.nio.file.Path
import js7.agent.RunningAgent
import js7.agent.scheduler.{AgentServerEvent, AgentServerState}
import js7.base.crypt.silly.{SillySignature, SillySignatureVerifier, SillySigner}
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.{autoClosing, multipleAutoClosing}
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.Akkas
import js7.common.configutils.Configs._
import js7.common.jsonseq.InputStreamJsonSeqReader
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.UntilNoneIterator
import js7.controller.RunningController
import js7.controller.data.events.ControllerEvent
import js7.data.agent.AgentId
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{<-:, Event, EventId, KeyedEvent, Stamped}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{VersionId, VersionedEvent}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.RecoveryTest._
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class RecoveryTest extends AnyFreeSpec
{
  // TODO Starte Controller und Agenten in eigenen Prozessen, die wir abbrechen können.

  "test" in {
    for (_ <- if (sys.props contains "test.infinite") Iterator.from(1) else Iterator(1)) {
      val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), TestWorkflow.path)
      val Seq(order1, order2, order3) = orders
      var lastEventId = EventId.BeforeFirst
      val directoryProvider = new DirectoryProvider(
        AgentIds,
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
          import directoryProvider.toSigned
          assert(controller.eventWatch.await[VersionedEvent]().map(_.value).sortBy(_.toString) ==
            Vector(
              NoKey <-: VersionAdded(VersionId("INITIAL")),
              NoKey <-: VersionedItemAdded(toSigned(TestWorkflow)),
              NoKey <-: VersionedItemAdded(toSigned(QuickWorkflow)))
            .sortBy(_.toString))
          runAgents(directoryProvider) { _ =>
            controller.addOrderBlocking(order1)
            controller.addOrderBlocking(QuickOrder)
            /*lastEventId =*/ lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == QuickOrder.id))
            lastEventId = lastEventIdOf(controller.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
            controller.addOrderBlocking(order2)
            lastEventId = lastEventIdOf(controller.eventWatch.await[OrderProcessed](after = lastEventId, predicate = _.key == order1.id))
          }
          val Vector(Stamped(_, _, NoKey <-: AgentServerEvent.ControllerRegistered(ControllerId("Controller")/*see default controller.conf*/, _, _/*agentRunId*/))) =
            readAgentEvents(directoryProvider.agents(0).dataDir / "state/agent--0.journal")

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
        logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
        try controller.terminate(suppressSnapshot = true) await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
      logger.info("🔥🔥🔥 TERMINATE CONTROLLER 🔥🔥🔥")
      // Kill Controller ActorSystem
      Akkas.terminateAndWait(controller.actorSystem, 99.s)
    }

  private def runAgents(directoryProvider: DirectoryProvider)(body: IndexedSeq[RunningAgent] => Unit): Unit =
    multipleAutoClosing(directoryProvider.agents.map(_.agentConfiguration) map RunningAgent.startForTest await 10.s) { agents =>
      // TODO Duplicate code in DirectoryProvider
      try body(agents)
      catch { case NonFatal(t) =>
        logger.error(t.toStringWithCauses) /* Akka may crash before the caller gets the error so we log the error here */
        try agents.map(_.terminate()) await 99.s
        catch { case t2: Throwable if t2 ne t => t.addSuppressed(t2) }
        throw t
      }
      logger.info("🔥🔥🔥 TERMINATE AGENTS 🔥🔥🔥")
      // Kill Agents ActorSystems
      for (agent <- agents) Akkas.terminateAndWait(agent.actorSystem, 99.s)
    }

  private def readAgentEvents(journalFile: Path): Vector[Stamped[KeyedEvent[AgentServerEvent]]] =
    autoClosing(InputStreamJsonSeqReader.open(journalFile)) { reader =>
      UntilNoneIterator(reader.read()).toVector
        .map(_.value)
        .collect {
          case json if AgentServerState.keyedEventJsonCodec canDeserialize json =>
            import AgentServerState.keyedEventJsonCodec
            json.as[Stamped[KeyedEvent[Event]]].orThrow
        }
        .collect {
          case o @ Stamped(_, _, KeyedEvent(_, _: AgentServerEvent)) => o.asInstanceOf[Stamped[KeyedEvent[AgentServerEvent]]]  // Ignore SnapshotTaken
        }
    }
}

private object RecoveryTest {
  private val logger = Logger(getClass)

  private val TestConfig = config"js7.journal.remove-obsolete-files = false"
  private val AgentIds = AgentId("agent-111") :: AgentId("agent-222") :: Nil
  private val TestPathExecutable = RelativePathExecutable("TEST.cmd", v1Compatible = true)

  private val TestWorkflow = Workflow(WorkflowPath("test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-0")),
      Execute(WorkflowJob.Name("TEST-1")),
      Execute(WorkflowJob.Name("TEST-1"))),
    Map(
      WorkflowJob.Name("TEST-0") -> WorkflowJob(AgentIds(0), TestPathExecutable, Map("var1" -> StringValue(s"VALUE-${AgentIds(0).string}"))),
      WorkflowJob.Name("TEST-1") -> WorkflowJob(AgentIds(1), TestPathExecutable, Map("var1" -> StringValue(s"VALUE-${AgentIds(1).string}")))))

  private val QuickWorkflow = Workflow.of(WorkflowPath("quick") ~ "INITIAL", Execute(WorkflowJob(AgentIds(0), TestPathExecutable)))
  private val QuickOrder = FreshOrder(OrderId("QUICK-ORDER"), QuickWorkflow.id.path)

  private val ExpectedOrderEvents = Vector(
    OrderAdded(TestWorkflow.id),
    OrderAttachable(AgentIds(0)),
    OrderAttached(AgentIds(0)),
    OrderStarted,
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-111")))),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderDetached,
    OrderAttachable(AgentIds(1)),
    OrderAttached(AgentIds(1)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-222")))),
    OrderMoved(Position(4)),
    OrderProcessingStarted,
    OrderStdoutWritten(StdoutOutput),
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("SCRIPT-VARIABLE-VALUE-agent-222")))),
    OrderMoved(Position(5)),
    OrderDetachable,
    OrderDetached,
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

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId = {
    val last = stamped.iterator.to(Vector).last
    logger.debug(s"lastEventIdOf => $last")
    last.eventId
  }
}
