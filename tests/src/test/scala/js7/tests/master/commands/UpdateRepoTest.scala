package js7.tests.master.commands

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
import js7.data.agent.AgentRefPath
import js7.data.event.{EventRequest, EventSeq}
import js7.data.filebased.Repo.ObjectVersionDoesNotMatchProblem
import js7.data.filebased.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.master.data.MasterCommand
import js7.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import js7.tests.master.commands.UpdateRepoTest._
import js7.tests.testenv.DirectoryProvider.Vinitial
import js7.tests.testenv.MasterAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = Nil

  override def beforeAll() = {
    (directoryProvider.master.configDir / "private" / "private.conf") ++=
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
    master.httpApi.login_(Some(UserAndPassword(UserId("without-permission"), SecretString("TEST-PASSWORD")))) await 99.s
    assert(executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)) ==
      Left(UserDoesNotHavePermissionProblem(UserId("without-permission"), UpdateRepoPermission)))

    master.httpApi.login_(Some(UserAndPassword(UserId("UpdateRepoTest"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "MasterCommand.UpdateRepo" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

    executeCommand(UpdateRepo(V2, sign(workflow2) :: Nil)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(1), TestWorkflowPath))

    val promises = Vector.fill(2)(Promise[Deadline]())
    for (i <- orderIds.indices) {
      master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderIds(i)) foreach {
        case EventSeq.NonEmpty(_) => promises(i).success(now)
        case o => fail(s"Unexpected: $o")
      }
    }
    val finishedAt = promises.map(_.future) await 99.s
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order

    executeCommand(UpdateRepo(V3, delete = TestWorkflowPath :: Nil)).orThrow
    assert(master.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.s) ==
      Left(Problem("Has been deleted: Workflow:/WORKFLOW~3")))

    withClue("Tampered with configuration: ") {
      val updateRepo = UpdateRepo(VersionId("vTampered"), sign(workflow2).copy(string = "TAMPERED") :: Nil)
      assert(executeCommand(updateRepo) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "MasterCommand.ReplaceRepo replaces all configuration objects" in {
    pending   // TODO Change of agents is not supported yet!
    // First, add two workflows
    executeCommand(UpdateRepo(V4, sign(workflow4) :: sign(otherWorkflow4) :: Nil)).orThrow
    locally {
      val checkedRepo = master.fileBasedApi.checkedRepo.await(99.s)
      assert(checkedRepo.map(_.versions) == Right(V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil))
      assert(checkedRepo.map(_.currentFileBaseds.toSet) ==
        Right(Set(workflow4 withVersion V4, otherWorkflow4 withVersion V4) ++ directoryProvider.agentRefs.map(_ withVersion Vinitial)))
    }

    // Now replace: delete one workflow and change the other
    executeCommand(ReplaceRepo(V5, otherWorkflow5 +: Nil/*directoryProvider.agentRefs.map(_ withVersion V5)*/ map sign)).orThrow
    val checkedRepo = master.fileBasedApi.checkedRepo.await(99.s)
    assert(checkedRepo.map(_.versions) == Right(V5 :: V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil))
    assert(checkedRepo.map(_.currentFileBaseds.toSet) ==
      Right(Set(otherWorkflow5 withVersion V5) ++ directoryProvider.agentRefs.map(_ withVersion V5)))

    val orderId = OrderId("⭕️")
    master.addOrderBlocking(FreshOrder(orderId, otherWorkflow5.path))

    val promise = Promise[Deadline]()
    master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderId) foreach {
      case EventSeq.NonEmpty(_) => promise.success(now)
      case o => fail(s"Unexpected: $o")
    }
    //...
  }

  "MasterCommand.UpdateRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(UpdateRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Left(ObjectVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow5.id)))
  }

  "MasterCommand.ReplaceRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(ReplaceRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Left(ObjectVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow5.id)))
  }

  private def executeCommand(cmd: MasterCommand): Checked[cmd.Response] =
    master.httpApi.executeCommand(cmd).map(Right.apply)
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
