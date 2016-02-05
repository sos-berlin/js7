package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.autoDeleting
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.taskserver.task.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import com.sos.scheduler.engine.taskserver.task.process.Processes._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
import java.io.IOException
import java.lang.ProcessBuilder.Redirect.PIPE
import java.nio.file.Files.exists
import java.nio.file.Paths
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessesTest extends FreeSpec {

  private val args = if (isWindows) List("1-one", "2-'two", "3\\three", "4-*")  // "-key=value" and several other strings does not work !!!
                     else List("1 one", "2 'two", "3 \"three", "4\\four", "5 *", "-key=value")

  "processToPidOption, toShellCommandArguments" in {
    if (isWindows) {
      val process = new ProcessBuilder(directShellCommandArguments("rem")).start()
      assert(processToPidOption(process).isEmpty)
      process.waitFor()
    } else {
      val args = directShellCommandArguments("echo $$")
      assert(args == List("/bin/sh", "-c", "echo $$"))
      val process = new ProcessBuilder(args).redirectInput(PIPE).start()
      val echoLine = io.Source.fromInputStream(process.getInputStream).getLines().next()
      assert(processToPidOption(process) contains Pid(echoLine.toLong))
      process.waitFor()
    }
  }

  "toShellCommandArguments" in {
    val file = Paths.get("FILE")
    val a = toShellCommandArguments(file, args)
    assert(a == List("FILE") ++ args)
    assert(toShellCommandArguments(file) == List("FILE"))  // Without arguments, it is shorter
  }

  "newTemporaryShellFile, toShellCommandArguments and script execution" in {
    autoDeleting(newTemporaryShellFile("NAME")) { file ⇒
      assert(exists(file))
      assert(!(file.toString contains "--"))
      file.contentString =
        if (isWindows) "@echo off\n" + (0 to args.size map { i ⇒ s"echo %$i\n" } mkString "")
        else 0 to args.size map { i ⇒ s"""echo "$$$i"""" + '\n' } mkString ""
      val process = new ProcessBuilder(toShellCommandArguments(file, args)).redirectOutput(PIPE).start()
      val echoLines = io.Source.fromInputStream(process.getInputStream).getLines().toList
      val expectedLines = List(file.toString) ++ args
      for ((a, b) ← echoLines zip expectedLines) assert(a == b)
      assert(echoLines.size == expectedLines.size)
      process.waitFor()
    }
  }

  "newLogFile" in {
    autoDeleting(newLogFile(temporaryDirectory, "NAME", Stdout)) { file ⇒
      assert(exists(file))
      assert(!(file.toString contains "--"))
    }
  }

  "TextFileBusyIOException" in {
    val (expected, exceptions) = List(
      true → new IOException("xx  error=26, Text file busy"),
      true → new IOException("xx  error=26, Das Programm kann nicht ausgeführt oder verändert werden (busy)"),
      true → new IOException("error=26"),
      false → new IOException("error=261")
    ).unzip
    val r = for (e ← exceptions) yield e match {
      case RobustlyStartProcess.TextFileBusyIOException(x) ⇒ assert(x eq e); true
      case _ ⇒ false
    }
    assert(r == expected)
  }
}
