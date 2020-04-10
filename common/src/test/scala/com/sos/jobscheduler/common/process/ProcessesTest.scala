package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.common.process.Processes._
import com.sos.jobscheduler.common.process.ProcessesTest._
import com.sos.jobscheduler.common.scalautil.FileUtils.autoDeleting
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax.RichPath
import com.sos.jobscheduler.common.system.FileUtils._
import com.sos.jobscheduler.common.system.OperatingSystem.{isMac, isSolaris, isWindows}
import com.sos.jobscheduler.data.system.Stdout
import java.io.IOException
import java.lang.ProcessBuilder.Redirect.PIPE
import java.nio.file.Files.exists
import java.nio.file.Paths
import org.scalatest.FreeSpec
import scala.jdk.CollectionConverters._

/**
 * @author Joacim Zschimmer
 */
final class ProcessesTest extends FreeSpec {

  "processToPidOption, toShellCommandArguments" in {
    if (isWindows) {
      val process = new ProcessBuilder(directShellCommandArguments("rem").asJava).start()
      assert(processToPidOption(process).isEmpty)
      process.waitFor()
    } else {
      val args = directShellCommandArguments("echo $$")
      assert(args == List("/bin/sh", "-c", "echo $$"))
      val process = new ProcessBuilder(args.asJava).redirectInput(PIPE).start()
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
      val process = new ProcessBuilder(toShellCommandArguments(file, Args).asJava).redirectOutput(PIPE).start()
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
}

private object ProcessesTest {
  // Different character combinations should not be changed (interpreted) by the operating systems shell script invoker.
  private val Args =
    if (isWindows)
      List("1-one", "2-'two", "3\\three", "4-*")  // "-key=value" and several other strings do not work !!!
    else
      List("1 one",
        "2 'two",
        "3 \"three",
        if (isSolaris || isMac)  // TestFailedException: "4[<FF>]our" did not equal "4[\f]our" - Solaris and macOS call to /bin/sh seems to interprete "\\f" as '\x0c' (FF)
          "4 four"  // Backslash is not useable as shell script argument !!!
        else "4\\four",
        "5 *",
        "-key=value")

  private val ShellScript =
    if (isWindows)
      "@echo off\r\n" +
        (0 to Args.size map { i => s"echo %$i\r\n" } mkString "")
    else
      0 to Args.size map { i => s"""echo "$$$i"""" + '\n' } mkString ""
}
