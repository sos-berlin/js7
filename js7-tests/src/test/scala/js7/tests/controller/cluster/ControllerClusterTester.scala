package js7.tests.controller.cluster

import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isWindows
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.tests.testenv.ControllerClusterForScalaTest.TestPathExecutable
import org.scalatest.freespec.AnyFreeSpec

private[cluster] trait ControllerClusterTester extends AnyFreeSpec with ControllerClusterForScalaTest
{
  protected def items = Seq(TestWorkflow)
  override protected def shellScript = ControllerClusterTester.shellScript
}

object ControllerClusterTester
{
  private[cluster] val TestWorkflow = WorkflowParser.parse(
    WorkflowPath("WORKFLOW"),
    s"""define workflow {
      |  execute executable="${TestPathExecutable.path}", agent="AGENT", v1Compatible=true, parallelism=2;
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
