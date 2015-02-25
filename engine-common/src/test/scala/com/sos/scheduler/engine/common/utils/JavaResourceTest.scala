package com.sos.scheduler.engine.common.utils

import com.google.common.io.Resources.getResource
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JavaResourceTest extends FreeSpec {

  private val path = "com/sos/scheduler/engine/common/utils/test.txt"
  private val nonExistentPath = "com/sos/scheduler/engine/common/utils/non-existent"

  "path" in {
    JavaResource(path).path shouldEqual path
    JavaResource(nonExistentPath).path shouldEqual nonExistentPath
  }

  "url" in {
    JavaResource(path).url shouldEqual getResource(path)
  }

  "requireExists" in {
    JavaResource(path).requireExistence()
    intercept[IllegalArgumentException] {
      JavaResource(nonExistentPath).requireExistence()
    }
  }
}
