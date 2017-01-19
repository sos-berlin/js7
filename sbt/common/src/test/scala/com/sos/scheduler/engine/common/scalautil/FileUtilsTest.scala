package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.{autoDeleting, withTemporaryFile}
import com.sos.scheduler.engine.common.scalautil.FileUtilsTest._
import java.io.File
import java.nio.charset.StandardCharsets.{UTF_16BE, UTF_8}
import java.nio.file.Files.{createTempDirectory, createTempFile, delete, exists}
import java.nio.file.{Files, NotDirectoryException, Path}
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FileUtilsTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val file = createTempFile("FileUtilTest-", ".tmp").toFile
  private lazy val path = file.toPath

  override def afterAll() = delete(file)

  "implicit fileToPath" in {
    new File("/a"): Path
  }

  "implicit pathToFile" in {
    new File("/a").toPath: File
  }

  "File extention methods" - {
    "slash" in {
      assert(new File("/a") / "b" == new File("/a", "b"))
    }

    "contentString" in {
      file.contentString = TestString
      file.contentString shouldEqual TestString
      new String(Files.readAllBytes(file), UTF_8) shouldEqual TestString
    }

    "contentBytes" in {
      file.contentBytes shouldEqual TestBytes
      file.contentBytes = Array[Byte](1, 2)
      file.contentBytes shouldEqual Vector[Byte](1, 2)
    }

    "write" in {
      file.write(TestString, UTF_16BE)
      file.contentBytes shouldEqual TestString.getBytes(UTF_16BE)
    }

    "append" in {
      file.append("X", UTF_16BE)
      file.contentString(UTF_16BE) shouldEqual TestString + "X"
    }
  }

  "Path extention methods" - {
    "slash" in {
      assert(new File("/a").toPath / "b" == new File("/a", "b").toPath)
    }

    "contentString" in {
      path.contentString = TestString
      path.contentString shouldEqual TestString
      new String(Files.readAllBytes(path), UTF_8) shouldEqual TestString
    }

    "contentBytes" in {
      path.contentBytes shouldEqual TestBytes
      path.contentBytes = Array[Byte](1, 2)
      path.contentBytes shouldEqual Vector[Byte](1, 2)
    }

    "write" in {
      path.write(TestString, UTF_16BE)
      path.contentBytes shouldEqual TestString.getBytes(UTF_16BE)
    }

    "append" in {
      path.append("X", UTF_16BE)
      path.contentString(UTF_16BE) shouldEqual TestString + "X"
    }

    "pathSet" in {
      intercept[NotDirectoryException] { path.pathSet }
      val dir = createTempDirectory("FileUtilsTest-")
      assert(dir.pathSet.isEmpty)
      val files = Set("a.tmp", "b.tmp") map dir.resolve
      files foreach { o ⇒ touch(o) }
      assert(dir.pathSet == files)
      files foreach delete
      delete(dir)
    }
  }

  "createShortNamedDirectory" in {
    assert(FileUtils.ShortNamePermutationCount == 2176782336L)
    val dir = createTempDirectory("test-")
    val n = 100
    val dirs = List.fill(n) {
      val prefix = "test-"
      val d = FileUtils.createShortNamedDirectory(dir, prefix)
      assert(Files.isDirectory(d))
      assert(d.getFileName.toString.length == prefix.length + 6)
      d
    }
    assert(dirs.toSet.size == n)
    dirs foreach delete
    delete(dir)
  }

  "withTemporaryFile" in {
    val f = withTemporaryFile { file ⇒
      assert(exists(file))
      file
    }
    assert(!exists(f))
  }

  "withTemporaryFile, named" in {
    val f = withTemporaryFile("TEST-", ".tmp") { file ⇒
      assert(exists(file))
      file
    }
    assert(!exists(f))
  }

  "autoDeleting" in {
    val file = createTempFile("TEST-", ".tmp")
    val a = autoDeleting(file) { f ⇒
      assert(file eq f)
      assert(exists(f))
      123
    }
    assert(a == 123)
    assert(!exists(file))
  }

  "autoDeleting with exception" in {
    val file = createTempFile("TEST-", ".tmp")
    intercept[IllegalStateException] {
      autoDeleting(file) { _ ⇒ throw new IllegalStateException }
    }
    assert(!exists(file))
  }
}

private object FileUtilsTest {
  private val TestString = "AÅ"
  private val TestBytes = TestString.getBytes(UTF_8)
  assert(TestBytes.length == 3)
}
