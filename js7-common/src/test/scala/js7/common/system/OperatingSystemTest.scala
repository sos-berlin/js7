package js7.common.system

import java.io.File
import java.io.File.pathSeparator
import js7.common.system.OperatingSystem._
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class OperatingSystemTest extends AnyFreeSpec {

  "makeModuleFilename" in {
    assert(makeModuleFilename("xx") == (if (isWindows) "xx.dll" else if (isMac) "libxx.dylib" else "libxx.so"))
  }

  "makeExecutableFilename" in {
    assert(makeExecutableFilename("xx") == (if (isWindows) "xx.exe" else "xx"))
  }

  "concatFileAndPathChain" in {
    val f = new File("/a/b")
    assertResult(List(f.getAbsolutePath, "x", "y", "z") mkString pathSeparator) {
      val path = List("x", "", "y", f.getAbsolutePath, "z") mkString pathSeparator
      concatFileAndPathChain(f, path)
    }
  }
}
