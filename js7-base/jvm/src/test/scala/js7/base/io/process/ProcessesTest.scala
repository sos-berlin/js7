package js7.base.io.process

import cats.instances.future.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import java.io.IOException
import java.lang.ProcessBuilder.Redirect.PIPE
import java.nio.file.Files.exists
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{autoDeleting, temporaryDirectory, withTemporaryFile}
import js7.base.io.process.Processes.*
import js7.base.io.process.ProcessesTest.*
import js7.base.system.OperatingSystem.{isMac, isSolaris, isWindows}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class ProcessesTest extends AnyFreeSpec {

  "processToPidOption, toShellCommandArguments" in {
    if (isWindows) {
      val process = new ProcessBuilder(directShellCommandArguments("rem").asJava)
        .startRobustly().await(99.s)
      assert(processToPidOption(process).isEmpty)
      process.waitFor()
    } else {
      val args = directShellCommandArguments("echo $$")
      assert(args == List("/bin/sh", "-c", "echo $$"))
      val process = new ProcessBuilder(args.asJava).redirectInput(PIPE)
        .startRobustly().await(99.s)
      val echoLine = scala.io.Source.fromInputStream(process.getInputStream).getLines().next()
      assert(processToPidOption(process) contains Pid(echoLine.toLong))
      process.waitFor()
    }
  }

  "toShellCommandArguments" in {
    val file = Paths.get("FILE")
    val a = toShellCommandArguments(file, Args)
    assert(a == List("FILE") ++ Args)
    assert(toShellCommandArguments(file) == List("FILE"))  // Without arguments, it is shorter
  }

  "newTemporaryShellFile, toShellCommandArguments and script execution" in {
    autoDeleting(newTemporaryShellFile("NAME")) { file =>
      assert(exists(file))
      assert(!(file.toString contains "--"))
      file := ShellScript
      val process = new ProcessBuilder(toShellCommandArguments(file, Args).asJava)
        .redirectOutput(PIPE)
        .startRobustly().await(99.s)
      val echoLines = scala.io.Source.fromInputStream(process.getInputStream).getLines().toList
      val normalizedFirstEcho = if (isWindows) echoLines.head stripSuffix "\"" stripPrefix "\"" else echoLines.head  // Windows (with sbt?) may echo the quoted file path
      assert(normalizedFirstEcho == file.toString)
      for ((a, b) <- echoLines.tail zip Args) assert(a == b)
      assert(echoLines.size - 1 == Args.size)
      process.waitFor()
    }
  }

  "newLogFile" in {
    autoDeleting(newLogFile(temporaryDirectory, "NAME", Stdout)) { file =>
      assert(exists(file))
      assert(!(file.toString contains "--"))
    }
  }

  "TextFileBusyIOException" in {
    val (expected, exceptions) = List(
      true -> new IOException("xx  error=26, Text file busy"),
      true -> new IOException("xx  error=26, Das Programm kann nicht ausgeführt oder verändert werden (busy)"),
      true -> new IOException("error=26"),
      false -> new IOException("error=261")
    ).unzip
    val r = for (e <- exceptions) yield e match {
      case RobustlyStartProcess.TextFileBusyIOException(x) => assert(x eq e); true
      case _ => false
    }
    assert(r == expected)
  }

  "Many empty shell script processes" in {
    for (n <- sys.props.get("test.speed").flatMap(o => Try(o.toInt).toOption)) {
      withTemporaryFile("ProcessesTest-", ".sh") { file =>
        file.writeUtf8Executable(":")
        val since = now
        (1 to n).toVector
          .traverse(_ => Future {
            runProcess(s"""'$file'""")
          })
          .await(99.s)
        info(Stopwatch.durationAndPerSecondString(since.elapsed, n, "processes"))
      }
    }
  }

  "Many processes" in {
    for (n <- sys.props.get("test.speed").flatMap(o => Try(o.toInt).toOption)) {
        val since = now
        (1 to n).toVector
          .traverse(_ => Future {
            runProcess("sleep 0")
          })
          .await(99.s)
        info(Stopwatch.durationAndPerSecondString(since.elapsed, n, "processes"))
    }
  }
  //"runProcess" in {
  //  assert(runProcess("echo HELLO").trim == "HELLO")
  //}
}

private object ProcessesTest
{
  // Different character combinations should not be changed (interpreted) by the operating systems shell script invoker.
  private val Args =
    if (isWindows)
      List("1-one", "2-'two", "3\\three", "4-*")  // "--key=value" and several other strings do not work !!!
    else
      List("1 one",
        "2 'two",
        "3 \"three",
        if (isSolaris || isMac)  // TestFailedException: "4[<FF>]our" did not equal "4[\f]our" - Solaris and macOS call to /bin/sh seems to interprete "\\f" as '\x0c' (FF)
          "4 four"  // Backslash is not useable as shell script argument !!!
        else "4\\four",
        "5 *",
        "--key=value")

  private val ShellScript =
    if (isWindows)
      "@echo off\r\n" +
        (0 to Args.size map { i => s"echo %$i\r\n" } mkString "")
    else
      0 to Args.size map { i => s"""echo "$$$i"""" + '\n' } mkString ""
}
