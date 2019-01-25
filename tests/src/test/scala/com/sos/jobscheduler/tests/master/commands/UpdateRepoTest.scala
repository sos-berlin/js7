package com.sos.jobscheduler.tests.master.commands

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutures
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTask
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem.sleepingShellScript
import com.sos.jobscheduler.core.message.ProblemCodeMessages
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
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.master.commands.UpdateRepoTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoTest extends FreeSpec
{
  ProblemCodeMessages.initialize()

  private val tick = 1.5.second

  "MasterCommand.UpdateRepo" in {
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil)) { directoryProvider ⇒
      import directoryProvider.fileBasedSigner.sign

      (directoryProvider.master.config / "private" / "private.conf") ++=
         """jobscheduler.auth.users {
           |  UpdateRepoTest {
           |    password = "plain:TEST-PASSWORD"
           |    permissions = "UpdateRepo"
           |  }
           |  without-permission {
           |    password = "plain:TEST-PASSWORD"
           |  }
           |}
           |""".stripMargin
      directoryProvider.agentToTree(TestAgentPath).writeExecutable(ExecutablePath("/SCRIPT1.cmd"), sleepingShellScript(2 * tick))
      directoryProvider.agentToTree(TestAgentPath).writeExecutable(ExecutablePath("/SCRIPT2.cmd"), ":")
      directoryProvider.run { (master, _) ⇒
        def executeCommand(cmd: MasterCommand): Checked[cmd.Response] =
          master.httpApi.executeCommand(cmd).map(Valid.apply)
            .onErrorRecover { case e: HttpException if e.problem.isDefined ⇒ Invalid(e.problem.get) }
            .await(99.seconds)

        withClue("Permission is required: ") {
          master.httpApi.login(Some(UserAndPassword(UserId("without-permission"), SecretString("TEST-PASSWORD")))) await 99.seconds
          assert(executeCommand(UpdateRepo(Some(versionId1), sign(workflow1) :: Nil)) ==
            Invalid(Problem("User does not have the required permission 'UpdateRepo'")))
        }

        master.httpApi.login(Some(UserAndPassword(UserId("UpdateRepoTest"), SecretString("TEST-PASSWORD")))) await 99.seconds

        val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
        executeCommand(UpdateRepo(Some(versionId1), sign(workflow1) :: Nil)).orThrow
        master.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

        executeCommand(UpdateRepo(Some(versionId2), sign(workflow2) :: Nil)).orThrow
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
        assert(finishedAt(0) > finishedAt(1) + tick)  // The second added order running on workflow version 2 finished before the first added order

        executeCommand(UpdateRepo(Some(VersionId("3")), delete = Set(TestWorkflowPath))).orThrow
        assert(master.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.seconds) ==
          Invalid(Problem("Has been deleted: Workflow:/WORKFLOW 3")))

        withClue("Tampered with configuration: ") {
          val updateRepo = UpdateRepo(
            Some(VersionId("4")),
            sign(workflow2).copy(message = "TAMPERED") :: Nil)
          assert(executeCommand(updateRepo) == Invalid(PGPTamperedWithMessageProblem))
        }
      }
    }
  }
}

object UpdateRepoTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestWorkflowPath = WorkflowPath("/WORKFLOW")
  private val script1 = """
    define workflow {
      execute executable="/SCRIPT1.cmd", agent="/AGENT";
    }"""

  private val versionId1 = VersionId("1")
  private val workflow1 = WorkflowParser.parse(TestWorkflowPath, script1).orThrow

  private val versionId2 = VersionId("2")
  private val script2 = """
    define workflow {
      execute executable="/SCRIPT2.cmd", agent="/AGENT";
    }"""
  private val workflow2 = WorkflowParser.parse(TestWorkflowPath, script2).orThrow
}
