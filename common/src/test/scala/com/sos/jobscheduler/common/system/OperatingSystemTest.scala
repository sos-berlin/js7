package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.system.OperatingSystem._
import java.io.File
import java.io.File.pathSeparator
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class OperatingSystemTest extends FreeSpec {

  "makeModuleFilename" in {
    assert(makeModuleFilename("xx") == (if (isWindows) "xx.dll" else "libxx.so"))
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
