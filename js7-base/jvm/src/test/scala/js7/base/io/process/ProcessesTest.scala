package js7.base.io.process

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.instances.vector.*
import cats.syntax.traverse.*
import java.lang.ProcessBuilder.Redirect.PIPE
import java.nio.file.Files.exists
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{autoDeleting, temporaryDirectory, withTemporaryFile}
import js7.base.io.process.Processes.*
import js7.base.io.process.ProcessesTest.*
import js7.base.io.process.StartRobustly.startRobustly
import js7.base.system.OperatingSystem.{isMac, isSolaris, isWindows}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class ProcessesTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "directShellCommandArguments" in:
    if isWindows then
      val process = new ProcessBuilder(directShellCommandArguments("rem").asJava)
        .startRobustly().await(99.s)
      process.waitFor()
      succeed
    else
      val args = directShellCommandArguments("echo $$")
      assert(args == List("/bin/sh", "-c", "echo $$"))
      val process = new ProcessBuilder(args.asJava).redirectInput(PIPE)
        .startRobustly().await(99.s)
      val echoLine = scala.io.Source.fromInputStream(process.getInputStream).getLines().next()
      process.waitFor()
      succeed

  "toShellCommandArguments" in:
    val file = Paths.get("FILE")
    val a = toShellCommandArguments(file, Args)
    assert(a == List("FILE") ++ Args)
    assert(toShellCommandArguments(file) == List("FILE"))  // Without arguments, it is shorter

  "newTemporaryShellFile, toShellCommandArguments and script execution" in:
    autoDeleting(newTemporaryShellFile("NAME")): file =>
      assert(exists(file))
      assert(!(file.toString contains "--"))
      file := ShellScript
      val process = new ProcessBuilder(toShellCommandArguments(file, Args).asJava)
        .redirectOutput(PIPE)
        .startRobustly().await(99.s)
      val echoLines = scala.io.Source.fromInputStream(process.getInputStream).getLines().toList
      val normalizedFirstEcho = if isWindows then echoLines.head stripSuffix "\"" stripPrefix "\"" else echoLines.head  // Windows (with sbt?) may echo the quoted file path
      assert(normalizedFirstEcho == file.toString)
      for (a, b) <- echoLines.tail zip Args do assert(a == b)
      assert(echoLines.size - 1 == Args.size)
      process.waitFor()
      succeed

  "Many empty shell script processes" in:
    for n <- sys.props.get("test.speed").flatMap(o => Try(o.toInt).toOption) do
      withTemporaryFile("ProcessesTest-", ".sh"): file =>
        file.writeUtf8Executable(":")
        val since = now
        (1 to n).toVector
          .traverse(_ => IO:
            runProcess(s"""'$file'"""))
          .await(99.s)
        info(Stopwatch.durationAndPerSecondString(since.elapsed, n, "processes"))

  "Many processes" in:
    for n <- sys.props.get("test.speed").flatMap(o => Try(o.toInt).toOption) do
      val since = now
      (1 to n).toVector
        .traverse(_ => IO:
          runProcess("sleep 0"))
        .await(99.s)
      info(Stopwatch.durationAndPerSecondString(since.elapsed, n, "processes"))
  //"runProcess" in {
  //  assert(runProcess("echo HELLO").trim == "HELLO")
  //}

private object ProcessesTest:
  // Different character combinations should not be changed (interpreted) by the operating systems shell script invoker.
  private val Args =
    if isWindows then
      List("1-one", "2-'two", "3\\three", "4-*")  // "--key=value" and several other strings do not work !!!
    else
      List("1 one",
        "2 'two",
        "3 \"three",
        if isSolaris || isMac then  // TestFailedException: "4[<FF>]our" did not equal "4[\f]our" - Solaris and macOS call to /bin/sh seems to interprete "\\f" as '\x0c' (FF)
          "4 four"  // Backslash is not useable as shell script argument !!!
        else "4\\four",
        "5 *",
        "--key=value")

  private val ShellScript =
    if isWindows then
      "@echo off\r\n" +
        (0 to Args.size map { i => s"echo %$i\r\n" } mkString "")
    else
      0 to Args.size map { i => s"""echo "$$$i"""" + '\n' } mkString ""
