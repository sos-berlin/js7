package js7.proxy.javaapi.data.command

import java.util.Arrays.asList
import java.util.Optional
import js7.data.command.CancelMode
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.proxy.javaapi.data.common.VavrUtils.getOrThrow
import js7.proxy.javaapi.data.workflow.JWorkflowId
import js7.proxy.javaapi.data.workflow.position.{JPosition, JWorkflowPosition}
import org.scalatest.freespec.AnyFreeSpec

final class JCancelModeTest extends AnyFreeSpec
{
  "JCancelMode" in {
    assert(JCancelMode.freshOnly.asScala == CancelMode.FreshOnly)
    assert(JCancelMode.kill(immediately = false).asScala ==
      CancelMode.FreshOrStarted(Some(CancelMode.Kill(immediately = false))))
    assert(JCancelMode.kill(immediately = true).asScala ==
      CancelMode.FreshOrStarted(Some(CancelMode.Kill(immediately = true))))
    assert(
      JCancelMode.kill(
        immediately = false,
        Optional.of(
          JWorkflowPosition.of(
            JWorkflowId.of("/WORKFLOW", "1.0"),
            getOrThrow(JPosition.fromList(asList(0, "Then", 1)))))
      ).asScala ==
        CancelMode.FreshOrStarted(Some(CancelMode.Kill(
          immediately = false,
          Some(WorkflowPath("/WORKFLOW") ~ "1.0" /: (Position(0) / "Then" % 1 ))))))
  }

  "Java" in {
    JCancelModeTester.test()
  }
}
