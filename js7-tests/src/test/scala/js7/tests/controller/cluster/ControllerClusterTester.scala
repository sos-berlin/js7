package js7.tests.controller.cluster

import js7.base.problem.Checked._
import js7.common.system.OperatingSystem.isWindows
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.testenv.ControllerClusterForScalaTest
import org.scalatest.freespec.AnyFreeSpec

private[cluster] trait ControllerClusterTester extends AnyFreeSpec with ControllerClusterForScalaTest
{
  override protected def inventoryItems = TestWorkflow :: Nil
  override protected def shellScript = ControllerClusterTester.shellScript
}

object ControllerClusterTester
{
  private[cluster] val TestWorkflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW"),
    """define workflow {
      |  execute executable="/TEST.cmd", agent="AGENT", taskLimit=2;
      |}""".stripMargin).orThrow

  private val shellScript = {
    val stdoutSize = 1_000_000
    val line = "." * 999
    (if (isWindows)
      """@echo off
        |if not "%SCHEDULER_PARAM_SLEEP" == "" ping -n 2 127.0.0.1 >nul
        |if not "%SCHEDULER_PARAM_SLEEP" == "" ping -n %SCHEDULER_PARAM_SLEEP 127.0.0.1 >nul
        |""".stripMargin
     else
      """[ -z "$SCHEDULER_PARAM_SLEEP" ] || sleep $SCHEDULER_PARAM_SLEEP
        |""".stripMargin
    ) +
      (1 to stdoutSize / line.length)
        .map(i => "echo " + s"$i $line".take(line.length) + "\n")
        .mkString
  }
}
