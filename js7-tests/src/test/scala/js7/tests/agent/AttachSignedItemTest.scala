package js7.tests.agent

import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachSignedItem, CreateAgent}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.SimpleUser
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.item.ItemRevision
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.agent.AttachSignedItemTest._
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AttachSignedItemTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "AttachSignedItem command verifies signature" in {
    import directoryProvider.itemSigner
    directoryProvider.runAgents() { case Seq(runningAgent) =>
      val agentApi = runningAgent.api(CommandMeta(SimpleUser(directoryProvider.agents(0).userAndPassword.get.userId)))
      assert(agentApi.commandExecute(CreateAgent(controllerId, agentPath)).await(99.s).toOption.get
        .isInstanceOf[CreateAgent.Response])

      // Signed VersionedItem
      val signedWorkflow = itemSigner.sign(workflow)
      assert(agentApi.commandExecute(AttachSignedItem(signedWorkflow)).await(99.s)
        == Right(AgentCommand.Response.Accepted))

      // Tampered VersionedItem
      val signedWorkflow2 = itemSigner.sign(workflow.copy(source = Some("TAMPERED")))
      val tamperedWorkflow = signedWorkflow2.copy(
        signedString = signedWorkflow2.signedString.copy(
          string = signedWorkflow2.signedString.string + " "))
      assert(agentApi.commandExecute(AttachSignedItem(tamperedWorkflow)).await(99.s)
        == Left(TamperedWithSignedMessageProblem))

      // SignableSimpleItem
      val signedJobResource = itemSigner.sign(jobResource.copy(itemRevision = Some(ItemRevision(1))))
      assert(agentApi.commandExecute(AttachSignedItem(signedJobResource)).await(99.s)
        == Right(AgentCommand.Response.Accepted))

      // Tampered SignableSimpleItem
      val signedSimpleItem2 = itemSigner.sign(jobResource.copy(
        itemRevision = Some(ItemRevision(2)),
        env = Map("ENV" -> StringConstant("TAMPERED"))))
      val tamperedSimpleItem = signedSimpleItem2.copy(
        signedString = signedSimpleItem2.signedString.copy(
          string = signedSimpleItem2.signedString.string + " "))
      assert(agentApi.commandExecute(AttachSignedItem(tamperedSimpleItem)).await(99.s)
        == Left(TamperedWithSignedMessageProblem))
    }
  }
}

object AttachSignedItemTest
{
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION")
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
}
