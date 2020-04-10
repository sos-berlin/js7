package com.sos.jobscheduler.common.scalautil

import cats.effect.SyncIO
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.common.scalautil.JavaSyncResources.fileAsResource
import java.io.{BufferedReader, InputStreamReader}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaSyncResourcesTest extends AnyFreeSpec
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
