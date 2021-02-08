package js7.common.scalautil

import cats.effect.SyncIO
import java.io.{BufferedReader, InputStreamReader}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.common.scalautil.JavaSyncResources.fileAsResource
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
