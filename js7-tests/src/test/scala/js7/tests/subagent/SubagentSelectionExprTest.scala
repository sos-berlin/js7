package js7.tests.subagent

import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
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
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.BlockingItemUpdater
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import fs2.Stream

final class SubagentSelectionExprTest
extends OurTestSuite, SubagentTester, BlockingItemUpdater:

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

  lazy val (bareSubagent, bareSubagentRelease) = subagentResource(bareSubagentItem)
    .allocated.await(99.s)

  override def beforeAll() =
    super.beforeAll()
    bareSubagent

  override def afterAll() =
    try 
      bareSubagentRelease.await(99.s)
    finally
      super.afterAll()

  "Expression references a non-existing variable" in:
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

  "Variable expression denotes an unknown SubagentSelection" in:
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

  "Invalid SubagentId syntax in constant expression is detected early" in:
    val workflow = Workflow(
      WorkflowPath("INVALID-SYNTAX") ~ "INVALID-SYNTAX",
      Seq(
        EmptyJob.execute(
          agentPath,
          subagentSelectionId = Some(expr("'*INVALID*'")))))
    val checked = controller.api
      .updateItems(Stream(
        AddVersion(workflow.id.versionId),
        AddOrChangeSigned(toSignedString(workflow))))
      .await(99.s)
    assert(checked == Left(Problem(
      "JSON DecodingFailure at : InvalidName: Invalid character or character combination in " +
        "name of SubagentSelectionId type: *INVALID*")))

  "subagentSelection=SUBAGENT-SELECTION" in:
    val orderId = OrderId("SELECTION")
    val freshOrder = FreshOrder(orderId, workflowPath, Map(
      "subagentSelectionId" -> StringValue(subagentSelectionId.string)))
    controller.addOrderBlocking(freshOrder)
    assert(
      eventWatch.await[OrderProcessingStarted](_.key == orderId).head.value.event.subagentId ==
        Some(bareSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId)

  "subagentSelection=SUBAGENT" in:
    val orderId = OrderId("SUBAGENT-ID-AS-SELECTION")
    val freshOrder = FreshOrder(orderId, workflowPath, Map(
      "subagentSelectionId" -> StringValue(bareSubagentId.string)))
    controller.addOrderBlocking(freshOrder)
    assert(
      eventWatch.await[OrderProcessingStarted](_.key == orderId).head.value.event.subagentId ==
        Some(bareSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId)


object SubagentSelectionExprTest:
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val subagentSelectionId = SubagentSelectionId("SUBAGENT-SELECTION")
