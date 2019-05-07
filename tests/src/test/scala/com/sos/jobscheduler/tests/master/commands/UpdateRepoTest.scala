package com.sos.jobscheduler.tests.master.commands

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutures
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTask
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem.sleepingShellScript
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.core.problems.TamperedWithSignedMessageProblem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import com.sos.jobscheduler.tests.master.commands.UpdateRepoTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.Vinitial
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = Nil

  override def beforeAll() = {
    (directoryProvider.master.config / "private" / "private.conf") ++=
       """jobscheduler.auth.users {
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
    master.httpApi.login(Some(UserAndPassword(UserId("without-permission"), SecretString("TEST-PASSWORD")))) await 99.s
    assert(executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)) ==
      Invalid(Problem("User 'without-permission' does not have the required permission 'UpdateRepo'")))

    master.httpApi.login(Some(UserAndPassword(UserId("UpdateRepoTest"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "MasterCommand.UpdateRepo" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    executeCommand(UpdateRepo(V1, sign(workflow1) :: Nil)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

    executeCommand(UpdateRepo(V2, sign(workflow2) :: Nil)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(1), TestWorkflowPath))

    val promises = Vector.fill(2)(Promise[Timestamp]())
    for (i <- orderIds.indices) {
      master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderIds(i)) foreach {
        case EventSeq.NonEmpty(_) =>
          promises(i).success(now)
      }
    }
    val finishedAt = promises.map(_.future) await 99.s
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order

    executeCommand(UpdateRepo(V3, delete = TestWorkflowPath :: Nil)).orThrow
    assert(master.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.s) ==
      Invalid(Problem("Has been deleted: Workflow:/WORKFLOW 3")))

    withClue("Tampered with configuration: ") {
      val updateRepo = UpdateRepo(VersionId("vTampered"), sign(workflow2).copy(string = "TAMPERED") :: Nil)
      assert(executeCommand(updateRepo) == Invalid(TamperedWithSignedMessageProblem))
    }
  }

  "MasterCommand.ReplaceRepo replaces all configuration objects" in {
    pending   // TODO Change of agents is not supported yet!
    // First, add two workflows
    executeCommand(UpdateRepo(V4, sign(workflow4) :: sign(otherWorkflow4) :: Nil)).orThrow
    locally {
      val repo = master.fileBasedApi.stampedRepo.await(99.s).value
      assert(repo.versions == V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil)
      assert(repo.currentFileBaseds.toSet
        == Set(workflow4 withVersion V4, otherWorkflow4 withVersion V4) ++ directoryProvider.agentRefs.map(_ withVersion Vinitial))
    }

    // Now replace: delete one workflow and change the other
    executeCommand(ReplaceRepo(V5, otherWorkflow5 +: Nil/*directoryProvider.agentRefs.map(_ withVersion V5)*/ map sign)).orThrow
    val repo = master.fileBasedApi.stampedRepo.await(99.s).value
    assert(repo.versions == V5 :: V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil)
    assert(repo.currentFileBaseds.toSet
      == Set(otherWorkflow5 withVersion V5) ++ directoryProvider.agentRefs.map(_ withVersion V5))

    val orderId = OrderId("⭕️")
    master.addOrderBlocking(FreshOrder(orderId, otherWorkflow5.path))

    val promise = Promise[Timestamp]()
    master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderId) foreach {
      case EventSeq.NonEmpty(_) =>
        promise.success(now)
    }
  }

  "MasterCommand.UpdateRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(UpdateRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Invalid(Problem("Expected version 'DIVERGE' in 'Workflow:/OTHER-WORKFLOW 5'")))
  }

  "MasterCommand.ReplaceRepo with divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(executeCommand(ReplaceRepo(VersionId("DIVERGE"), sign(otherWorkflow5) :: Nil))
      == Invalid(Problem("Expected version 'DIVERGE' in 'Workflow:/OTHER-WORKFLOW 5'")))
  }

  private def executeCommand(cmd: MasterCommand): Checked[cmd.Response] =
    master.httpApi.executeCommand(cmd).map(Valid.apply)
      .onErrorRecover { case e: HttpException if e.problem.isDefined => Invalid(e.problem.get) }
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
