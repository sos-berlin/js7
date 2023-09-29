package js7.data_for_java.command

import java.util.Arrays.asList
import java.util.Optional
import js7.base.test.OurTestSuite
import js7.data.command.CancellationMode
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data_for_java.vavr.VavrUtils.getOrThrow
import js7.data_for_java.workflow.JWorkflowId
import js7.data_for_java.workflow.position.{JPosition, JWorkflowPosition}

final class JCancellationModeTest extends OurTestSuite:
  "JCancellationMode" in:
    assert(JCancellationMode.freshOnly.asScala == CancellationMode.FreshOnly)
    assert(JCancellationMode.kill(immediately = false).asScala ==
      CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(immediately = false))))
    assert(JCancellationMode.kill(immediately = true).asScala ==
      CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(immediately = true))))
    assert(
      JCancellationMode.kill(
        immediately = false,
        Optional.of(
          JWorkflowPosition.of(
            JWorkflowId.of("WORKFLOW", "1.0"),
            getOrThrow(JPosition.fromList(asList(0, "Then", 1)))))
      ).asScala ==
        CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(
          immediately = false,
          Some(WorkflowPath("WORKFLOW") ~ "1.0" /: (Position(0) / "Then" % 1 ))))))

  "Java" in:
    JCancellationModeTester.test()
