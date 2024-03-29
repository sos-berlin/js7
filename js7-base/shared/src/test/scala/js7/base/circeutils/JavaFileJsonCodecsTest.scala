package js7.base.circeutils

import java.io.File
import java.nio.file.Paths
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.JavaFileJsonCodecs.PathJsonCodec
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class JavaFileJsonCodecsTest extends OurTestSuite:
  "Path" in:
    if File.separatorChar == '\\' then
      testJson(Paths.get("/tmp/test"), json""" "\\tmp\\test" """)
    else
      testJson(Paths.get("/tmp/test"), json""" "/tmp/test" """)
