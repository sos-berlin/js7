package js7.data.value.expression.scopes

import cats.instances.vector._
import cats.syntax.parallel._
import java.io.IOException
import java.nio.file.Files.{isRegularFile, newOutputStream}
import java.nio.file.StandardOpenOption.{APPEND, WRITE}
import java.nio.file.{AccessDeniedException, Path, Paths}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{Argument, StringConstant}
import js7.data.value.expression.scopes.FileValueScope.functionName
import js7.data.value.expression.scopes.FileValueScopeTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.util.Random

final class FileValueScopeTest extends AnyFreeSpec
{
  "IOException" in {
    val dir = Paths.get("/tmp/FileValueScopeTest-NonExistant")
    autoClosing(new FileValueState(dir)) { fileValueState =>
      val (fileValueScope, release) = FileValueScope.resource(fileValueState).allocated.await(99.s)
      assert(
        toFile(fileValueScope, Seq("CONTENT"))
          .left
          .exists(_.throwable.isInstanceOf[IOException]))
      release.await(99.s)
    }
  }

  "Arbitrary filename" in {
    check { fileValueScope =>
      val n = 3

      for (i <- 1 to n) {
        val content = s"CONTENT-$i"
        val file = toFile(fileValueScope, Seq(content)).orThrow
        assert(file.startsWith(fileValueScope.fileValueState.directory))
        assert(file.contentString == content)
      }

      val files = (fileValueScope.fileValueState.directory / "0").directoryContents
      assert(files.size == n)
      for (f <- files) assert(isRegularFile(f))
    }
  }

  "Fixed filename" in {
    check { fileValueScope =>
      val file = toFile(fileValueScope, Seq("CONTENT", "FIXED-NAME")).orThrow
      assert(file == fileValueScope.fileValueState.directory / "0" / "FIXED-NAME")
      assert(file.contentString == "CONTENT")

      val files = (fileValueScope.fileValueState.directory / "0").directoryContents
      assert(files.size == 1)
      for (f <- files) assert(isRegularFile(f))
    }
  }

  "Filename pattern" in {
    check { fileValueScope =>
      val file = toFile(fileValueScope, Seq("CONTENT", "PREFIX-*.tmp")).orThrow
      assert(file.startsWith(fileValueScope.fileValueState.directory))
      assert(file.getFileName.toString.startsWith("PREFIX-"))
      assert(file.getFileName.toString.endsWith(".tmp"))
      assert(file.contentString == "CONTENT")
      assert(fileValueScope.fileValueState.directory.directoryContents.size == 1)
    }
  }

  "Slash in filenamePattern is rejected" in {
    check { fileValueScope =>
      assert(toFile(fileValueScope, Seq("CONTENT", "myDir/*")) == Left(Problem(
        "No directory is allowed in toFile function filenamePattern argument")))
    }
  }

  "Multiple * in filenamePattern" in {
    check { fileValueScope =>
      val file = toFile(fileValueScope, Seq("", "*;*;*")).orThrow
      val filename = file.getFileName.toString
      val star = filename.substring(0, filename.indexOf(';'))
      assert(filename == s"$star;$star;$star")
    }
  }

  "File is read-only" in {
    check { fileValueScope =>
      val file = toFile(fileValueScope, Seq("READ-ONLY", "*")).orThrow
      intercept[AccessDeniedException] {
        newOutputStream(file, WRITE, APPEND)
      }
    }
  }

  "toFile is concurrently executable" in {
    // Some different filenames, used multiple times.
    // May also test a future cache.
    val stringCount = 100
    val strings = (1 to stringCount).map(_.toString)

    withTemporaryDirectory("FileValueScopeTest-") { dir =>
      autoClosing(new FileValueState(dir)) { fileValueState =>
        val n = 1000
        val m = 10
        val t = now
        val files: Seq[Path] =
          (1 to n)
            .toVector
            .parFlatTraverse(_ =>
              FileValueScope
                .resource(fileValueState)
                .use(fileValueScope =>
                  Task.shift *>
                    Task
                      .parTraverse((1 to m).toVector)(_ => Task {
                        val string = strings(Random.nextInt(stringCount))
                        toFile(fileValueScope, Seq(string, string)).orThrow
                      })
                      .tapEval(files =>
                        Task.shift *>
                          Task(assert(files
                            .forall(file => file.contentString == file.getFileName.toString))))))
            .await(99.s)

        assert(files.size == n * m)
        logger.info(itemsPerSecondString(t.elapsed, n * m, "calls"))

        assert(fileValueState.isEmpty)
      }
      assert(dir.directoryContents.isEmpty)
    }
  }

  private def toFile(fileValueScope: FileValueScope, args: Seq[String]): Checked[Path] =
    Expression.FunctionCall(functionName, args.map(a => Argument(StringConstant(a))))
      .evalAsString(fileValueScope)
      .map(Paths.get(_))

  private def check(body: FileValueScope => Unit): Unit =
    withTemporaryDirectory("FileValueScopeTest-") { dir =>
      autoClosing(new FileValueState(dir)) { fileValueState =>
        val (fileValueScope, release) = FileValueScope.resource(fileValueState).allocated.await(99.s)
        assert(dir.directoryContents.isEmpty)
        body(fileValueScope)
        release.await(99.s)
        assert(fileValueState.isEmpty)
      }
      assert(dir.directoryContents.isEmpty)
    }
}

object FileValueScopeTest {
  private val logger = Logger[this.type]
}
