package js7.common.scalautil

import cats.effect.SyncIO
import java.io.{BufferedReader, InputStreamReader}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.OurTestSuite
import js7.common.scalautil.JavaSyncResources.fileAsResource

/**
  * @author Joacim Zschimmer
  */
final class JavaSyncResourcesTest extends OurTestSuite
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
