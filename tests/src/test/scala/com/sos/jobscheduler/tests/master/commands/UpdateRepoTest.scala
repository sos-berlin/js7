package com.sos.jobscheduler.tests.master.commands

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
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
import com.sos.jobscheduler.core.problems.PGPTamperedWithMessageProblem
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.DirectoryProvider.Vinitial
import com.sos.jobscheduler.tests.master.commands.UpdateRepoTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  import directoryProvider.fileBasedSigner.sign

  protected val agentPaths = TestAgentPath :: Nil

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
    directoryProvider.agentToTree(TestAgentPath).writeExecutable(ExecutablePath("/SCRIPT1.cmd"), sleepingShellScript(2 * Tick))
    directoryProvider.agentToTree(TestAgentPath).writeExecutable(ExecutablePath("/SCRIPT2.cmd"), ":")
    directoryProvider.agentToTree(TestAgentPath).writeExecutable(ExecutablePath("/SCRIPT4.cmd"), ":")
    super.beforeAll()
  }

  "User requires permission 'UpdateRepo'" in {
    master.httpApi.login(Some(UserAndPassword(UserId("without-permission"), SecretString("TEST-PASSWORD")))) await 99.seconds
    assert(executeCommand(UpdateRepo(sign(workflow1) :: Nil, versionId = V1.some)) ==
      Invalid(Problem("User does not have the required permission 'UpdateRepo'")))

    master.httpApi.login(Some(UserAndPassword(UserId("UpdateRepoTest"), SecretString("TEST-PASSWORD")))) await 99.seconds
  }

  "MasterCommand.UpdateRepo" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    executeCommand(UpdateRepo(sign(workflow1) :: Nil, versionId = V1.some)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

    executeCommand(UpdateRepo(sign(workflow2) :: Nil, versionId = V2.some)).orThrow
    master.addOrderBlocking(FreshOrder(orderIds(1), TestWorkflowPath))

    val promises = Vector.fill(2)(Promise[Timestamp]())
    for (i ← orderIds.indices) {
      master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = 99.seconds), _.key == orderIds(i)) foreach {
        case EventSeq.NonEmpty(_) ⇒
          promises(i).success(now)
      }
    }
    val finishedAt = promises.map(_.future) await 99.seconds
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order

    executeCommand(UpdateRepo(delete = TestWorkflowPath :: Nil, versionId = V3.some)).orThrow
    assert(master.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.seconds) ==
      Invalid(Problem("Has been deleted: Workflow:/WORKFLOW 3")))

    withClue("Tampered with configuration: ") {
      val updateRepo = UpdateRepo(sign(workflow2).copy(message = "TAMPERED") :: Nil)
      assert(executeCommand(updateRepo) == Invalid(PGPTamperedWithMessageProblem))
    }
  }

  "MasterCommand.ReplaceRepo replaces all configuration objects" in {
    val originalAgents = directoryProvider.agentFileBased.map(_ withVersion Vinitial)

    // First, add two workflows
    executeCommand(UpdateRepo(sign(workflow4) :: sign(otherWorkflow4) :: Nil, versionId = V4.some)).orThrow
    locally {
      val repo = master.fileBasedApi.stampedRepo.await(99.seconds).value
      assert(repo.versions == V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil)
      assert(repo.currentFileBaseds.toSet == Set(workflow4 withVersion V4, otherWorkflow4 withVersion V4) ++ originalAgents)
    }

    // Now replace: delete one workflow and change the other
    executeCommand(ReplaceRepo(otherWorkflow5 +: directoryProvider.agentFileBased map sign, versionId = V5.some)).orThrow
    val repo = master.fileBasedApi.stampedRepo.await(99.seconds).value
    assert(repo.versions == V5 :: V4 :: V3 :: V2 :: V1 :: Vinitial :: Nil)
    assert(repo.currentFileBaseds.toSet == Set(otherWorkflow5 withVersion V5) ++ originalAgents)

    val orderId = OrderId("⭕️")
    master.addOrderBlocking(FreshOrder(orderId, otherWorkflow5.path))

    val promise = Promise[Timestamp]()
    master.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = 99.seconds), _.key == orderId) foreach {
      case EventSeq.NonEmpty(_) ⇒
        promise.success(now)
    }
  }

  private def executeCommand(cmd: MasterCommand): Checked[cmd.Response] =
    master.httpApi.executeCommand(cmd).map(Valid.apply)
      .onErrorRecover { case e: HttpException if e.problem.isDefined ⇒ Invalid(e.problem.get) }
      .await(99.seconds)
}

object UpdateRepoTest
{
  private val Tick = 1.5.second
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestWorkflowPath = WorkflowPath("/WORKFLOW")
  private val script1 = """
    define workflow {
      execute executable="/SCRIPT1.cmd", agent="/AGENT";
    }"""

  private val V1 = VersionId("1")
  private val workflow1 = WorkflowParser.parse(TestWorkflowPath, script1).orThrow

  private val V2 = VersionId("2")
  private val script2 = """
    define workflow {
      execute executable="/SCRIPT2.cmd", agent="/AGENT";
    }"""
  private val workflow2 = WorkflowParser.parse(TestWorkflowPath, script2).orThrow

  private val V3 = VersionId("3")

  private val V4 = VersionId("4")
  private val workflow4 = WorkflowParser.parse(TestWorkflowPath, script2).orThrow
  private val otherWorkflow4 = WorkflowParser.parse(WorkflowPath("/OTHER-WORKFLOW"), script2).orThrow

  private val V5 = VersionId("5")
  private val script5 = """
    define workflow {
      execute executable="/SCRIPT4.cmd", agent="/AGENT";
    }"""
  private val otherWorkflow5 = WorkflowParser.parse(WorkflowPath("/OTHER-WORKFLOW"), script5).orThrow
}
