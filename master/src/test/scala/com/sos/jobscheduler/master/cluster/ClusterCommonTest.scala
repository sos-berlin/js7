package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.master.cluster.ClusterCommon._
import java.nio.file.Files.{delete, size}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterCommonTest extends FreeSpec
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
