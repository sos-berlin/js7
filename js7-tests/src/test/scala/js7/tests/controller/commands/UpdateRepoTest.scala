package js7.tests.controller.commands

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{UpdateRepoPermission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits.RichFutures
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.common.system.OperatingSystem.operatingSystem.sleepingShellScript
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{RemoveOrdersWhenTerminated, ReplaceRepo, UpdateRepo}
import js7.data.Problems.{ItemDeletedProblem, ItemVersionDoesNotMatchProblem}
import js7.data.agent.AgentRefPath
import js7.data.event.{EventRequest, EventSeq}
import js7.data.item.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.tests.controller.commands.UpdateRepoTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.Vinitial
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val inventoryItems = Nil

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
       """js7.auth.users {
         |  UpdateRepoTest {
         |    password = "plain:TEST-PASSWORD"
         |    permissions = [ UpdateRepo ]
         |  }
         |  without-permission {
         |    password = "plain:TEST-PASSWORD"
         |  }
         |}
         |""".stripMargin
    directoryProvider.agentToTree(TestAgentRefPath).writeExecutable(ExecutablePath("/SCRIPT1.cmd"), sleepingShellScript(2 * Tick))
    directoryProvider.agentToTree(TestAgentRefPath).writeExecutable(ExecutablePath("/SCRIPT2.cmd"), ":")
    directoryProvider.agentToTree(TestAgentRefPath).writeExecutable(ExecutablePath("/SCRIPT4.cmd"), ":")
    super.beforeAll()
  }

  "User requires permission 'UpdateRepo'" in {
    controller.httpApi.login_(Some(UserAndPassword(UserId("without-permission"), SecretString("TEST-PASSWORD")))) await 99.s
    assert(executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)) ==
      Left(UserDoesNotHavePermissionProblem(UserId("without-permission"), UpdateRepoPermission)))

    controller.httpApi.login_(Some(UserAndPassword(UserId("UpdateRepoTest"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "ControllerCommand.UpdateRepo" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)).orThrow
    controller.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

    executeCommand(UpdateRepo(V2, sign(workflow2) :: Nil)).orThrow
    controller.addOrderBlocking(FreshOrder(orderIds(1), TestWorkflowPath))

    val promises = Vector.fill(2)(Promise[Deadline]())
    for (i <- orderIds.indices) {
      controller.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderIds(i)) foreach {
        case EventSeq.NonEmpty(_) => promises(i).success(now)
        case o => fail(s"Unexpected: $o")
      }
    }
    val finishedAt = promises.map(_.future) await 99.s
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(orderIds)).await(99.s).orThrow

    executeCommand(UpdateRepo(V3, delete = TestWorkflowPath :: Nil)).orThrow
    assert(controller.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.s) ==
      Left(ItemDeletedProblem(TestWorkflowPath)))

    withClue("Tampered with configuration: ") {
      val updateRepo = UpdateRepo(VersionId("vTampered"), sign(workflow2).copy(string = "TAMPERED") :: Nil)
      assert(executeCommand(updateRepo) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "ControllerCommand.ReplaceRepo replaces all configuration objects" in {
    pending   // TODO Change of agents is not supported yet!
    // First, add two workflows
    executeCommand(UpdateRepo(V4, sign(workflow4) :: sign(otherWorkflow4) :: Nil)).orThrow
    locally {
      val checkedRepo = controller.itemApi.checkedRepo.await(99.s)
      assert(checkedRepo.map(_.versions) == Right(V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil))
      assert(checkedRepo.map(_.currentItems.toSet) ==
        Right(Set(workflow4 withVersion V4, otherWorkflow4 withVersion V4) ++ directoryProvider.agentRefs.map(_ withVersion Vinitial)))
    }

    // Now replace: delete one workflow and change the other
    executeCommand(ReplaceRepo(V5, otherWorkflow5 +: Nil/*directoryProvider.agentRefs.map(_ withVersion V5)*/ map sign)).orThrow
    val checkedRepo = controller.itemApi.checkedRepo.await(99.s)
    assert(checkedRepo.map(_.versions) == Right(V5 :: V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil))
    assert(checkedRepo.map(_.currentItems.toSet) ==
      Right(Set(otherWorkflow5 withVersion V5) ++ directoryProvider.agentRefs.map(_ withVersion V5)))

    val orderId = OrderId("⭕️")
    controller.addOrderBlocking(FreshOrder(orderId, otherWorkflow5.path))

    val promise = Promise[Deadline]()
    controller.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderId) foreach {
      case EventSeq.NonEmpty(_) => promise.success(now)
      case o => fail(s"Unexpected: $o")
    }
    //...
  }

  "ControllerCommand.UpdateRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(UpdateRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Left(ItemVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow5.id)))
  }

  "ControllerCommand.ReplaceRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(ReplaceRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Left(ItemVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow5.id)))
  }

  private def executeCommand(cmd: ControllerCommand): Checked[cmd.Response] =
    controller.httpApi.executeCommand(cmd).map(Right.apply)
      .onErrorRecover { case e: HttpException if e.problem.isDefined => Left(e.problem.get) }
      .await(99.s)
}

object UpdateRepoTest
{
  private val Tick = 2.s
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflowPath = WorkflowPath("/WORKFLOW")
  private val script1 = """
    define workflow {
      execute executable="/SCRIPT1.cmd", agent="/AGENT";
    }"""

  private val V1 = VersionId("1")
  private val workflow1 = WorkflowParser.parse(TestWorkflowPath ~ V1, script1).orThrow

  private val V2 = VersionId("2")
  private val script2 = """
    define workflow {
      execute executable="/SCRIPT2.cmd", agent="/AGENT";
    }"""
  private val workflow2 = WorkflowParser.parse(TestWorkflowPath ~ V2, script2).orThrow

  private val V3 = VersionId("3")

  private val V4 = VersionId("4")
  private val workflow4 = WorkflowParser.parse(TestWorkflowPath ~ V4, script2).orThrow
  private val otherWorkflow4 = WorkflowParser.parse(WorkflowPath("/OTHER-WORKFLOW") ~ V4, script2).orThrow

  private val V5 = VersionId("5")
  private val script5 = """
    define workflow {
      execute executable="/SCRIPT4.cmd", agent="/AGENT";
    }"""
  private val otherWorkflow5 = WorkflowParser.parse(WorkflowPath("/OTHER-WORKFLOW") ~ V5, script5).orThrow
}
