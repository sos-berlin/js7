package js7.common.system

import java.io.File
import java.io.File.pathSeparator
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.test.OurTestSuite
import js7.common.system.ServerOperatingSystem.*

/**
 * @author Joacim Zschimmer
 */
final class ServerOperatingSystemTest extends OurTestSuite:

  "makeModuleFilename" in:
    assert(makeModuleFilename("xx") == (if isWindows then "xx.dll" else if isMac then "libxx.dylib" else "libxx.so"))

  "makeExecutableFilename" in:
    assert(makeExecutableFilename("xx") == (if isWindows then "xx.exe" else "xx"))

  "concatFileAndPathChain" in:
    val f = new File("/a/b")
    assertResult(List(f.getAbsolutePath, "x", "y", "z") mkString pathSeparator):
      val path = List("x", "", "y", f.getAbsolutePath, "z") mkString pathSeparator
      concatFileAndPathChain(f, path)
