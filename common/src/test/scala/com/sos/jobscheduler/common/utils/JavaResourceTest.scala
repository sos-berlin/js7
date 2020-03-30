package com.sos.jobscheduler.common.utils

import cats.effect.SyncIO
import com.google.common.io.Resources.getResource
import com.google.common.io.{ByteStreams, Resources}
import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.utils.JavaResourceTest._
import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files.{createTempDirectory, createTempFile, delete}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class JavaResourceTest extends FreeSpec
{
  "Macro derives ClassLoader" in {
    assert(JavaResource(getClass.getClassLoader, "MISSING").classLoader eq getClass.getClassLoader)
  }

  "simpleName" in {
    javaResource.simpleName shouldEqual "test.txt"
    nonExistentJavaResource.simpleName shouldEqual "non-existent"
  }

  "path" in {
    javaResource.path shouldEqual path
    nonExistentJavaResource.path shouldEqual nonExistentPath
  }

  "toString" in {
    javaResource.toString shouldEqual path
    nonExistentJavaResource.toString shouldEqual nonExistentPath
  }

  "asUtf8String" in {
    assert(javaResource.asUTF8String == expectedString)
  }

  "contentBytes" in {
    assert(javaResource.contentBytes sameElements expectedString.getBytes(UTF_8.name))
  }

  "copyToFile" in {
    val tmp = createTempFile("test", ".tmp")
    intercept[FileAlreadyExistsException] { javaResource.copyToFile(tmp) }
    javaResource.copyToFile(tmp, REPLACE_EXISTING) should be theSameInstanceAs tmp
    assert(tmp.contentString == expectedString)
    delete(tmp)
  }

  "copyToFiles" in {
    val dir = createTempDirectory("test")
    val files = JavaResource(getClass.getClassLoader, dirPath).copyToFiles(List("test.txt", "test-2.txt"), dir)
    assert(files == List(dir / "test.txt", dir / "test-2.txt"))
    assert(files(0).contentString == expectedString)
    assert(files(1).contentString == "TEST 2\n")
    files foreach delete
    delete(dir)
  }

  "url" in {
    javaResource.url shouldEqual getResource(path)
  }

  "openStream" in {
    autoClosing(javaResource.openStream()) { in =>
      assert(ByteStreams.toByteArray(in).toSeq == Resources.toByteArray(javaResource.url).toSeq)
    }
  }

  "/" in {
    assert((JavaResource(getClass.getClassLoader, "some/directory") / "resource").path == "some/directory/resource")
    assert((JavaResource(getClass.getClassLoader, "some/directory/") / "resource").path == "some/directory/resource")
  }

  "requireExists" in {
    javaResource.requireExistence()
    intercept[ProblemException] {
      nonExistentJavaResource.requireExistence()
    }
  }

  "asResource (Cats Effect)" in {
    val io = javaResource.asResource.use(in =>
      SyncIO {
        new BufferedReader(new InputStreamReader(in)).readLine()
      })
    assert(io.unsafeRunSync() == expectedString.stripSuffix("\n"))
  }

  "Implicit cats.effect.Resource" in {
    val io = javaResource.use(in =>
      SyncIO {
        new BufferedReader(new InputStreamReader(in)).readLine()
      })
    assert(io.unsafeRunSync() == expectedString.stripSuffix("\n"))
  }
}

object JavaResourceTest
{
  private val dirPath = "com/sos/jobscheduler/common/utils"
  private val path = "com/sos/jobscheduler/common/utils/test.txt"
  private val expectedString = "TEST CONTENT IN -> UTF-8\n"
  private val nonExistentPath = "com/sos/jobscheduler/common/utils/non-existent"
  private val javaResource = JavaResource(getClass.getClassLoader, path)
  private val nonExistentJavaResource = JavaResource(getClass.getClassLoader, nonExistentPath)
}
