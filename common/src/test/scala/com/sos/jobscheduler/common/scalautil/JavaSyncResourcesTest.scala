package com.sos.jobscheduler.common.scalautil

import cats.effect.SyncIO
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.common.scalautil.JavaSyncResources.fileAsResource
import java.io.{BufferedReader, InputStreamReader}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaSyncResourcesTest extends FreeSpec
{
  "use" in {
    withTemporaryFile { file =>
      val io = fileAsResource(file).use(in =>
        SyncIO {
          new BufferedReader(new InputStreamReader(in)).readLine()
        })
      file := "CONTENT"
      assert(io.unsafeRunSync() == "CONTENT")
      file := "OTHER"
      assert(io.unsafeRunSync() == "OTHER")
    }
  }
}
