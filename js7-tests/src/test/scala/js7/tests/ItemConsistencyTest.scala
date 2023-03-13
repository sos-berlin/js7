package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.data.Problems.MissingReferencedItemProblem
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.VersionId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.LockInstruction
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ItemConsistencyTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

final class ItemConsistencyTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  protected def agentPaths = Nil
  protected def items = Nil

  import directoryProvider.itemSigner

  "Adding a FileWatch with missing Workflow is rejected" in {
    val checked = controller.api
      .updateItems(Observable(AddOrChangeSimple(fileWatch)))
      .await(99.s)
    assert(checked == Left(Problem.Combined(Seq(
      MissingReferencedItemProblem(fileWatch.path, agentPath),
      MissingReferencedItemProblem(fileWatch.path, workflow.path)))))
  }

  "Workflow consistency" in {
    val checked = controller.api
      .updateItems(Observable(
        AddVersion(versionId),
        AddOrChangeSigned(itemSigner.sign(workflow).signedString)))
      .await(99.s)
    assert(checked == Left(Problem.Combined(Set(
      MissingReferencedItemProblem(workflow.id, lock.path),
      MissingReferencedItemProblem(workflow.id, jobResource.path),
      MissingReferencedItemProblem(workflow.id, agentPath)))))

    controller.api
      .updateItems(Observable(
        AddOrChangeSimple(AgentRef(agentPath, Seq(subagentId))),
        AddOrChangeSimple(SubagentItem(subagentId, agentPath, Uri("http://0.0.0.0:0"))),
        AddOrChangeSimple(lock),
        AddOrChangeSigned(itemSigner.sign(jobResource).signedString),
        AddVersion(versionId),
        AddOrChangeSigned(itemSigner.sign(workflow).signedString)))
      .await(99.s)
      .orThrow
  }

  "Adding the FileWatch" in {
    controller.api
      .updateItems(Observable(AddOrChangeSimple(fileWatch)))
      .await(99.s)
      .orThrow
  }
}

object ItemConsistencyTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")
  private val lock = Lock(LockPath("LOCK"))
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val versionId = VersionId("1")
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ versionId,
    Vector(
      LockInstruction.single(lock.path, None, Workflow.of(
        EmptyJob.execute(agentPath,
          jobResourcePaths = Seq(jobResource.path))))))

  private val fileWatch = FileWatch(OrderWatchPath("ORDER-WATCH"), workflow.path, agentPath,
    expr("'/dev/null'"))
}
