package js7.launcher.process

import cats.effect.unsafe.IORuntime
import java.io.InputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.*
import java.nio.file.Path
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, temporaryDirectory}
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.RobustlyStartProcess
import js7.base.io.process.{Pid, Processes}
import js7.base.log.Logger
import js7.base.system.OperatingSystem.{isMac, isSolaris, isUnix, isWindows}
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.job.TaskId
import js7.launcher.process.ProcessKillScriptTest.*
import scala.concurrent.duration.*
import scala.concurrent.{Future, blocking}
import scala.jdk.CollectionConverters.*

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "Kill script kills descendants" in:
    if isMac then
      info("Disabled on macOS because it kills our builder process")
      pending
    else
      val out = createTempFile("test-", ".log")
      val (scriptFile, process) = startNestedProcess(TestTaskId, out)
      sleep(2.s)
      logProcessTree()
      runKillScript(TestTaskId, Pid(process.pid))
      process.waitFor(10, SECONDS)
      assert(process.exitValue == SIGKILLexitValue)
      sleep(1.s) // Time to let kill take effect
      val beforeKill = out.contentString
      logger.info(s"\n" + beforeKill)
      if isUnix then
        assert(beforeKill contains "TEST-1=")
        assert(beforeKill contains "TEST-2=")
        assert(beforeKill contains "TEST-3=")
      sleep(2.s)
      logger.info("All processes should be killed now")
      logProcessTree()
      val grown = out.contentString stripPrefix beforeKill
      assert(grown == "", "Stdout file must not grow after kill script execution")
      delete(scriptFile)
      delete(out)
      succeed

  private def startNestedProcess(taskId: TaskId, out: Path): (Path, Process) =
    val file = Processes.newTemporaryShellFile("test")
    file := script
    val args = List(file.toString, s"--agent-task-id=${taskId.string}")
    val process = new ProcessBuilder(args.asJava).redirectOutput(out).redirectError(INHERIT)
      .startRobustly().await(99.s)
    logger.info(s"Started process ${process.pid}")
    (file, process)

  private def runKillScript(taskId: TaskId, pid: Pid): Unit =
    autoClosing(new ProcessKillScriptProvider): provider =>
      val tmp = createTempDirectory("test-")
      val killScript = provider.provideTo(temporaryDirectory)
      val args = killScript.toCommandArguments(taskId, pid)
      val killProcess = new ProcessBuilder(args.asJava).startRobustly().await(99.s)
      startLogStreams(killProcess, "Kill script") await 60.s
      killProcess.waitFor(60, SECONDS)
      assert(killProcess.exitValue == 0)
      deleteDirectoryRecursively(tmp)

  private def logProcessTree(): Unit =
    if isUnix && !isMac then
      val ps = new ProcessBuilder("ps", "fux").startRobustly().await(99.s)
      startLogStreams(ps, "ps (for information only, please ignore errors)") await 15.s
      ps.waitFor()

  private def startLogStreams(process: Process, prefix: String): Future[Any] =
    Future.sequence(List(
      Future[Unit] { blocking { logStream(process.getInputStream, s"$prefix stdout") }},
      Future[Unit] { blocking { logStream(process.getErrorStream, s"$prefix stderr") }}))


private object ProcessKillScriptTest:

  private val logger = Logger[this.type]
  private val TestTaskId = TaskId("1-TEST")
  private def script =
    val resource =
      if isWindows then
        JavaResource("js7/launcher/process/scripts/windows/test.cmd")
      else
        JavaResource("js7/launcher/process/scripts/unix/test.sh")
    resource.asUTF8String

  private val SIGKILLexitValue =
    if isWindows then 1
    else if isSolaris then SIGKILL.number
    else 128 + SIGKILL.number

  private def logStream(in: InputStream, prefix: String): Unit =
    logger.info("\n" + readLines(in, prefix).mkString("\n"))

  private def readLines(in: InputStream, prefix: String): Iterator[String] =
    for line <- scala.io.Source.fromInputStream(in).getLines() yield s"$prefix: $line"
