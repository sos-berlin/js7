package js7.controller.cluster

import java.nio.file.Files.{delete, size}
import java.nio.file.Paths
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryFile
import js7.controller.cluster.ClusterCommon._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterCommonTest extends AnyFreeSpec
{
  "truncateFile" in {
    withTemporaryFile("ClusterCommonTest", ".tmp") { file =>
      val a = "CONTENT\n"
      val b = (1 to 4906).map(_.toString).mkString(",")
      file := a + b
      assert(size(file) > a.length)

      truncateFile(file, a.length)
      assert(size(file) == a.length)
      assert(file.contentString == a)
      val truncated = Paths.get(file.toString + "~TRUNCATED-AFTER-FAILOVER")
      assert(truncated.contentString == b)

      delete(truncated)
    }
  }
}
