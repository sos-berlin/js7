package js7.data_for_java.workflow.position

import java.util.Arrays.asList
import js7.base.test.OurTestSuite
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position

final class JBranchPathTest extends OurTestSuite {
  "fromSeq empty" in {
    val emptyList = asList[Any]()
    val jBranchPath = JBranchPath.fromList(emptyList).get
    assert(jBranchPath.asScala.isEmpty)
    assert(jBranchPath == JBranchPath.empty)
    assert(jBranchPath.toString == "")
  }

  "fromSeq" in {
    val jBranchPath = JBranchPath.fromList(asList(1, "A", 2, "B")).get
    assert(jBranchPath.asScala == Position(1) / "A" % 2 / "B")
    assert(jBranchPath.toList == asList(1, "A", 2, "B"))
    assert(jBranchPath.toString == "1/A:2/B")
  }
}
