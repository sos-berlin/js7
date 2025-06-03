package js7.tests

import cats.effect.unsafe.IORuntime
import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessLostDueToRestartProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.JobIsNotRestartableTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.DirectoryProviderForScalaTest

final class JobIsNotRestartableTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "isNotRestartable OrderOutcome.Disrupted(ProcessLost(...))) and job restart" in:
    directoryProvider.runController(): controller =>
      directoryProvider.runAgents(): agents =>
        val Seq(agent) = agents
        val orderId = OrderId("JobIsNotRestartableTest")
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        controller.eventWatch.await[OrderStdoutWritten](_.key == orderId)

        // FailOver (no cluster here) lets the Agent terminate despite the order process
        agent.terminate(clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
          .await(99.s)

      directoryProvider.runAgents(): _ =>
        val orderId = OrderId("JobIsNotRestartableTest")
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        controller.eventWatch.await[OrderTerminated](_.key == orderId)

        val events = controller.eventWatch.eventsByKey[OrderEvent](orderId)
        assert(events == Seq(
          OrderAdded(workflow.id),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("STARTED\n"),
          // isNotRestartable = false would emit:
          // OrderProcessed(OrderOutcome.processLost(ProcessLostDueToRestartProblem)),
          OrderProcessed(OrderOutcome.Disrupted(ProcessLostDueToRestartProblem)),
          OrderDetachable,
          OrderDetached,
          OrderFailed(Position(0))))


object JobIsNotRestartableTest:

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val workflow = Workflow(
    WorkflowPath("JobIsNotRestartableTest") ~ "INITIAL",
    Seq(
      Execute.Anonymous:
        WorkflowJob(
          agentPath,
          ShellScriptExecutable:
            if isWindows then
              """@echo off
                |echo STARTED
                |ping -n 200 127.0.0.1 >nul
                |""".stripMargin
            else
              """#!/usr/bin/env bash
                |echo STARTED
                |sleep 199
                |""".stripMargin,
          isNotRestartable = true)))
