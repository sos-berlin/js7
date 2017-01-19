package com.sos.scheduler.engine.common.utils

import com.google.common.io.Resources.getResource
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.nio.charset.StandardCharsets
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files.{createTempDirectory, createTempFile, delete}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class JavaResourceTest extends FreeSpec {

  private val dirPath = "com/sos/scheduler/engine/common/utils"
  private val path = "com/sos/scheduler/engine/common/utils/test.txt"
  private val expectedString = "TEST CONTENT IN → UTF-8\n"
  private val nonExistentPath = "com/sos/scheduler/engine/common/utils/non-existent"

  "simpleName" in {
    JavaResource(path).simpleName shouldEqual "test.txt"
    JavaResource(nonExistentPath).simpleName shouldEqual "non-existent"
  }

  "path" in {
    JavaResource(path).path shouldEqual path
    JavaResource(nonExistentPath).path shouldEqual nonExistentPath
  }

  "toString" in {
    JavaResource(path).toString shouldEqual path
    JavaResource(nonExistentPath).toString shouldEqual nonExistentPath
  }

  "asUtf8String" in {
    assert(JavaResource(path).asUTF8String == expectedString)
  }

  "contentBytes" in {
    assert(JavaResource(path).contentBytes sameElements expectedString.getBytes(StandardCharsets.UTF_8.name))
  }

  "copyToFile" in {
    val tmp = createTempFile("test", ".tmp")
    intercept[FileAlreadyExistsException] { JavaResource(path).copyToFile(tmp) }
    JavaResource(path).copyToFile(tmp, REPLACE_EXISTING) should be theSameInstanceAs tmp
    assert(tmp.contentString == expectedString)
    delete(tmp)
  }

  "copyToFiles" in {
    val dir = createTempDirectory("test")
    val files = JavaResource(dirPath).copyToFiles(List("test.txt", "test-2.txt"), dir)
    assert(files == List(dir / "test.txt", dir / "test-2.txt"))
    assert(files(0).contentString == expectedString)
    assert(files(1).contentString == "TEST 2\n")
    files foreach delete
    delete(dir)
  }

  "url" in {
    JavaResource(path).url shouldEqual getResource(path)
  }

  "/" in {
    assert(JavaResource("some/directory") / "resource" == JavaResource("some/directory/resource"))
    assert(JavaResource("some/directory/") / "resource" == JavaResource("some/directory/resource"))
  }

  "requireExists" in {
    JavaResource(path).requireExistence()
    intercept[IllegalArgumentException] {
      JavaResource(nonExistentPath).requireExistence()
    }
  }
}
