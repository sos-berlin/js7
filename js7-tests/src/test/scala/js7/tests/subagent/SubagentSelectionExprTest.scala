package js7.tests.subagent

import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderOutcomeAdded, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.subagent.{SubagentSelection, SubagentSelectionId}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentSelectionExprTest.*
import js7.tests.testenv.BlockingItemUpdater
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler
import monix.reactive.Observable

final class SubagentSelectionExprTest
extends OurTestSuite with SubagentTester with BlockingItemUpdater
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(
    Workflow(
      workflowPath ~ "INITIAL",
      Seq(
        EmptyJob.execute(
          agentPath,
          subagentSelectionId = Some(expr("$subagentSelectionId")),
          processLimit = 100))),
    bareSubagentItem,
    SubagentSelection(subagentSelectionId, Map(
      localSubagentId -> 1,
      bareSubagentId -> 2)))

  protected implicit val scheduler = Scheduler.traced

  lazy val (bareSubagent, bareSubagentRelease) = subagentResource(bareSubagentItem)
    .allocated.await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    bareSubagent
  }

  override def afterAll() = {
    bareSubagentRelease.await(99.s)
    super.beforeAll()
  }

  "Expression references a non-existing variable" in {
    val orderId = OrderId("MISSING")
    val events = controller.runOrder(FreshOrder(orderId, workflowPath))
    assert(events.map(_.value) == Seq(
      OrderAdded(workflowPath ~ "INITIAL"),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderOutcomeAdded(Outcome.Disrupted(Problem("No such named value: subagentSelectionId"))),
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(0))))
  }

  "Variable expression denotes an unknown SubagentSelection" in {
    val orderId = OrderId("UNKNOWN")
    val events = controller.runOrder(FreshOrder(orderId, workflowPath, Map(
      "subagentSelectionId" -> StringValue("UNKNOWN-SUBAGENT-SELECTION"))))
    eventWatch.await[OrderFailed](_.key == orderId)
    assert(events.map(_.value) == Seq(
      OrderAdded(workflowPath ~ "INITIAL", Map(
        "subagentSelectionId" -> StringValue("UNKNOWN-SUBAGENT-SELECTION"))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderOutcomeAdded(Outcome.Disrupted(UnknownKeyProblem("SubagentId", "Subagent:UNKNOWN-SUBAGENT-SELECTION"))),
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(0))))
  }

  "Invalid SubagentId syntax in constant expression is detected early" in {
    val workflow = Workflow(
      WorkflowPath("INVALID-SYNTAX") ~ "INVALID-SYNTAX",
      Seq(
        EmptyJob.execute(
          agentPath,
          subagentSelectionId = Some(expr("'*INVALID*'")))))
    val checked = controllerApi
      .updateItems(Observable(
        AddVersion(workflow.id.versionId),
        AddOrChangeSigned(toSignedString(workflow))))
      .await(99.s)
    assert(checked == Left(Problem(
      "JSON DecodingFailure at : InvalidName: Invalid character or character combination in " +
        "name of SubagentSelectionId type: *INVALID*")))
  }

  "subagentSelection=SUBAGENT-SELECTION" in {
    val orderId = OrderId("SELECTION")
    val freshOrder = FreshOrder(orderId, workflowPath, Map(
      "subagentSelectionId" -> StringValue(subagentSelectionId.string)))
    controller.addOrder(freshOrder).await(99.s).orThrow
    assert(
      eventWatch.await[OrderProcessingStarted](_.key == orderId).head.value.event.subagentId ==
        Some(bareSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId)
  }

  "subagentSelection=SUBAGENT" in {
    val orderId = OrderId("SUBAGENT-ID-AS-SELECTION")
    val freshOrder = FreshOrder(orderId, workflowPath, Map(
      "subagentSelectionId" -> StringValue(bareSubagentId.string)))
    controller.addOrder(freshOrder).await(99.s).orThrow
    assert(
      eventWatch.await[OrderProcessingStarted](_.key == orderId).head.value.event.subagentId ==
        Some(bareSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId)
  }
}

object SubagentSelectionExprTest
{
  private val agentPath = AgentPath("AGENT")
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val subagentSelectionId = SubagentSelectionId("SUBAGENT-SELECTION")
}
