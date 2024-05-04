package js7.base.io.file

import cats.effect.{IO, SyncIO}
import io.circe.Json
import java.io.File.separator
import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.charset.StandardCharsets.{UTF_16BE, UTF_8}
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, createTempFile, delete, exists}
import java.nio.file.{Files, NotDirectoryException, Path, Paths}
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{autoDeleting, checkRelativePath, copyDirectoryContent, deleteDirectoryRecursively, provideFile, temporaryDirectoryResource, touchFile, withTemporaryDirectory, withTemporaryFile}
import js7.base.io.file.FileUtilsTest.*
import js7.base.problem.ProblemException
import js7.base.test.OurAsyncTestSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.{Assertion, BeforeAndAfterAll}

final class FileUtilsTest extends OurAsyncTestSuite, BeforeAndAfterAll:

  private lazy val path = createTempFile("FileUtilTest-", ".tmp")

  override def afterAll() =
    try
      delete(path)
    finally
      super.afterAll()

  "implicit fileToPath" in:
    new File("/a"): Path
    succeed

  "implicit pathToFile" in:
    new File("/a").toPath: File
    succeed

  "Path extention methods" - {
    "slash" - {
      val a = Paths.get("a")
      "valid" in:
        assert((a / "b").toString == s"a${separator}b")
      "invalid" - {
        for invalid <- InvalidRelativePaths do
          invalid in:
            intercept[ProblemException]:
              a / invalid
            succeed
      }
    }

    "contentString" in:
      path.contentString = TestString
      path.contentString shouldEqual TestString
      new String(Files.readAllBytes(path), UTF_8) shouldEqual TestString

    "contentBytes" in:
      path.contentBytes shouldEqual TestBytes
      path := Array[Byte](1, 2)
      path.contentBytes shouldEqual Vector[Byte](1, 2)

    "byteArray" in:
      path := Array[Byte](1, 2)
      path.byteArray shouldEqual ByteArray(1, 2)
      path := Array[Byte](7, 8)
      path.byteArray shouldEqual ByteArray(7, 8)

    "readAs" in:
      path := Array[Byte](1, 2)
      path.readAs[ByteArray] shouldEqual ByteArray(1, 2)
      path := Array[Byte](7, 8)
      path.readAs[ByteArray] shouldEqual ByteArray(7, 8)

    ":= String" in:
      path := TestString
      path.contentString shouldEqual TestString
      new String(Files.readAllBytes(path), UTF_8) shouldEqual TestString

    "++= String" in:
      path ++= "-APPENDED"
      path.contentString shouldEqual TestString + "-APPENDED"
      new String(Files.readAllBytes(path), UTF_8) shouldEqual TestString + "-APPENDED"

    ":= Array[Byte]" in:
      path := Array[Byte](1, 2)
      path.contentBytes shouldEqual Vector[Byte](1, 2)

    ":= Seq[Byte]" in:
      path := Seq[Byte](1, 2)
      path.contentBytes shouldEqual Vector[Byte](1, 2)

    ":= ByteArray" in:
      val bytes = "A-Å".getBytes(UTF_8)
      path := ByteArray(bytes)
      assert(path.byteArray == ByteArray(bytes))
      assert(path.contentBytes.toSeq == bytes.toSeq)

    "+= ByteArray" in:
      val complete = "A-Å-APPENDED".getBytes(UTF_8)
      val bytes = "-APPENDED".getBytes(UTF_8)
      path ++= ByteArray(bytes)
      assert(path.byteArray == ByteArray(complete))
      assert(path.contentBytes.toSeq == complete.toSeq)

    ":= JSON" in:
      path := Json.obj("key" -> Json.fromInt(7))
      path.contentString shouldEqual """{"key":7}"""

    "write" in:
      path.write(TestString, UTF_16BE)
      path.contentBytes shouldEqual TestString.getBytes(UTF_16BE)

    "append" in:
      path.append("X", UTF_16BE)
      path.contentString(UTF_16BE) shouldEqual TestString + "X"

    "directoryContentsAs" in:
      intercept[NotDirectoryException] { path.directoryContentsAs(Set) }
      val dir = createTempDirectory("FileUtilsTest-")
      assert(dir.directoryContentsAs(Set).isEmpty)
      val files = Set("a.tmp", "b.tmp") map dir.resolve
      files foreach { o => touchFile(o) }
      assert(dir.directoryContentsAs(Set) == files)
      assert(dir.directoryContentsAs(Set) == files)
      files foreach delete
      delete(dir)
      succeed
  }

  "inputStreamResource" in:
    withTemporaryFile { file =>
      val io = file.inputStreamResource[SyncIO].use(in =>
        SyncIO {
          new BufferedReader(new InputStreamReader(in)).readLine()
        })
      file := "CONTENT"
      assert(io.unsafeRunSync() == "CONTENT")
      file := "OTHER"
      assert(io.unsafeRunSync() == "OTHER")
    }

  "copyDirectoryContent" in:
    withTemporaryDirectory("FileUtilsTest-A-") { a =>
      a / "1" := 1
      createDirectories(a / "aa" / "aaa")
      a / "aa" / "aaa" / "2" := 2
      createDirectories(a / "ax")

      withTemporaryDirectory("FileUtilsTest-B-") { bParent =>
        val b = bParent / "B"
        copyDirectoryContent(a, b)
        assert((b / "1").contentString == "1")
        assert((b / "aa" / "aaa" / "2").contentString == "2")
        assert((b / "ax").isDirectory)
        deleteDirectoryRecursively(b)
      }
    }
    succeed

  "createShortNamedDirectory" in:
    assert(FileUtils.ShortNamePermutationCount == 2176782336L)
    val dir = createTempDirectory("test-")
    val n = 100
    val dirs = List.fill(n):
      val prefix = "test-"
      val d = FileUtils.createShortNamedDirectory(dir, prefix)
      assert(Files.isDirectory(d))
      assert(d.getFileName.toString.length == prefix.length + 6)
      d
    assert(dirs.toSet.size == n)
    dirs foreach delete
    delete(dir)
    succeed

  "withTemporaryFile" in:
    val f = withTemporaryFile { file =>
      assert(exists(file))
      file
    }
    assert(!exists(f))

  "withTemporaryFile, named" in:
    val f = withTemporaryFile("TEST-", ".tmp") { file =>
      assert(exists(file))
      file
    }
    assert(!exists(f))

  "temporaryDirectoryResource" in:
    var directory: Path = null
    temporaryDirectoryResource[IO]("FileUtilsTest-")
      .use(dir => IO {
        directory = dir
        touchFile(dir / "FILE")
        createDirectory(dir / "SUBDIRECTORY")
        touchFile(dir / "SUBDIRECTORY" / "FILE")
      })
      .*>(IO(assert(!exists(directory))))

  "autoDeleting" in:
    val file = createTempFile("TEST-", ".tmp")
    val a = autoDeleting(file) { f =>
      assert(file eq f)
      assert(exists(f))
      123
    }
    assert(a == 123)
    assert(!exists(file))

  "autoDeleting with exception" in:
    val file = createTempFile("TEST-", ".tmp")
    intercept[IllegalStateException]:
      autoDeleting(file) { _ => throw new IllegalStateException }
    assert(!exists(file))

  "checkRelativePath" - {
    "valid" in:
      assert(checkRelativePath("relative").isRight)

    "invalid" - {
      for invalid <- InvalidRelativePaths do
        invalid in:
          assert(checkRelativePath(invalid).isLeft)
    }
  }

  "provideFile" - {
    "Non-existing file" in:
      check(existing = false)

    "Existing file is delete before use" in:
      check(existing = true)

    def check(existing: Boolean): Assertion =
      withTemporaryFile("FileUtilsTest-", ".tmp") { file =>
        if !existing then delete(file)
        provideFile[SyncIO](file)
          .use(file => SyncIO {
            assert(!exists(file))
            file := "Hej!"
          })
          .unsafeRunSync()
        assert(!exists(file))
      }
  }

private object FileUtilsTest:
  private val TestString = "AÅ"
  private val TestBytes = TestString.getBytes(UTF_8)
  assert(TestBytes.length == 3)

  private val InvalidRelativePaths = List(
    "",
    "/b",
    """\b""",
    "./b",
    "../b",
    """.\b""",
    """..\b""",
    "b/./c",
    "b/../c",
    """b/.\c""",
    """b/..\c""",
    """b\.\c""",
    """b\..\c""",
    """b\./c""",
    """b\../c""",
    "b/.",
    "b/..",
    """b\.""",
    """b\..""")
