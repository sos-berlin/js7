package js7.cluster

import java.nio.file.Files.{delete, exists, size}
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class ClusterCommonTest extends Test
{
  "truncateFile keep=true" in {
    withTemporaryFile("ClusterCommonTest", ".tmp") { file =>
      val a = "CONTENT\n"
      val b = (1 to 4906).map(_.toString).mkString(",")
      file := a + b
      assert(size(file) > a.length)

      ClusterCommon.truncateFile(file, a.length, keep = true)
      assert(size(file) == a.length)
      assert(file.contentString == a)
      val truncated = Paths.get(file.toString + "~TRUNCATED-AFTER-FAILOVER")
      assert(truncated.contentString == b)

      delete(truncated)
    }
  }

  "truncateFile keep=false" in {
    withTemporaryFile("ClusterCommonTest", ".tmp") { file =>
      val a = "CONTENT\n"
      file := a + "REST"
      assert(size(file) > a.length)

      ClusterCommon.truncateFile(file, a.length, keep = false)
      assert(size(file) == a.length)
      assert(file.contentString == a)

      assert(!exists(Paths.get(file.toString + "~TRUNCATED-AFTER-FAILOVER")))
    }
  }
}
