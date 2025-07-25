package js7.tests.agent

import cats.effect.unsafe.IORuntime
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachSignedItem, DedicateAgentDirector}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.SimpleUser
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.JournalId
import js7.data.item.ItemRevision
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.agent.AttachSignedItemTest.*
import js7.tests.testenv.DirectoryProviderForScalaTest

final class AttachSignedItemTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "AttachSignedItem command verifies signature" in:
    import directoryProvider.itemSigner
    directoryProvider.runAgents(): agents =>
      val agent = agents.head
      agent.untilReady.await(99.s)
      val meta = CommandMeta:
        SimpleUser(directoryProvider.agentEnvs(0).controllerUserAndPassword.get.userId)
      val controllerRunId = ControllerRunId(JournalId.random())

      agent.executeCommand(
        DedicateAgentDirector(
          Seq(SubagentId("SUBAGENT")), controllerId, controllerRunId, agentPath),
        meta)
      .await(99.s).orThrow

      // Signed VersionedItem
      val signedWorkflow = itemSigner.sign(workflow)
      assert:
        agent.executeCommand(AttachSignedItem(signedWorkflow), meta).await(99.s)
          == Right(AgentCommand.Response.Accepted)

      // Tampered VersionedItem
      val signedWorkflow2 = itemSigner.sign(workflow.copy(source = Some("TAMPERED")))
      val tamperedWorkflow = signedWorkflow2.copy(
        signedString = signedWorkflow2.signedString.copy(
          string = signedWorkflow2.signedString.string + " "))
      assert:
        agent.executeCommand(AttachSignedItem(tamperedWorkflow), meta).await(99.s)
          == Left(TamperedWithSignedMessageProblem)

      // SignableSimpleItem
      val signedJobResource = itemSigner.sign(jobResource.copy(itemRevision = Some(ItemRevision(1))))
      assert:
        agent.executeCommand(AttachSignedItem(signedJobResource), meta).await(99.s)
          == Right(AgentCommand.Response.Accepted)

      // Tampered SignableSimpleItem
      val signedSimpleItem2 = itemSigner.sign(jobResource.copy(
        itemRevision = Some(ItemRevision(2)),
        env = Map("ENV" -> StringConstant("TAMPERED"))))
      val tamperedSimpleItem = signedSimpleItem2.copy(
        signedString = signedSimpleItem2.signedString.copy(
          string = signedSimpleItem2.signedString.string + " "))
      assert:
        agent.executeCommand(AttachSignedItem(tamperedSimpleItem), meta).await(99.s)
          == Left(TamperedWithSignedMessageProblem)


object AttachSignedItemTest:
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION")
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
