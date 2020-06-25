package js7.taskserver.task.process

import java.io.InputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files._
import java.nio.file.Path
import js7.agent.data.AgentTaskId
import js7.base.process.ProcessSignal.SIGKILL
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.process.Processes
import js7.common.process.Processes.{Pid, RobustlyStartProcess, processToPidOption}
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.implicits._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.system.FileUtils._
import js7.common.system.OperatingSystem.{isMac, isSolaris, isUnix, isWindows}
import js7.common.utils.JavaResource
import js7.taskserver.task.process.ProcessKillScriptTest._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}
import scala.jdk.CollectionConverters._

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends AnyFreeSpec {

  "Kill script kills descendants" in {
    if (isMac) {
      info("Disabled on MacOS because it kills our builder process")
      pending
    } else {
      val out = createTempFile("test-", ".log")
      val (scriptFile, process) = startNestedProcess(TestAgentTaskId, out)
      sleep(2.s)
      logProcessTree()
      runKillScript(TestAgentTaskId, processToPidOption(process))
      process.waitFor(10, SECONDS)
      assert(process.exitValue == SIGKILLexitValue)
      sleep(1.s) // Time to let kill take effect
      val beforeKill = out.contentString
      logger.info(s"\n" + beforeKill)
      if (isUnix) {
        assert(beforeKill contains "TEST-1=")
        assert(beforeKill contains "TEST-2=")
        assert(beforeKill contains "TEST-3=")
      }
      sleep(2.s)
      logger.info("All processes should be killed now")
      logProcessTree()
      val grown = out.contentString stripPrefix beforeKill
      assert(grown == "", "Stdout file must not grow after kill script execution")
      delete(scriptFile)
      delete(out)
    }
  }

  private def startNestedProcess(agentTaskId: AgentTaskId, out: Path): (Path, Process) = {
    val file = Processes.newTemporaryShellFile("test")
    file := Script
    val args = List(file.toString, s"--agent-task-id=${agentTaskId.string}")
    val process = new ProcessBuilder(args.asJava).redirectOutput(out).redirectError(INHERIT).startRobustly()
    logger.info(s"Started process ${processToPidOption(process)}")
    (file, process)
  }

  private def runKillScript(agentTaskId: AgentTaskId, pidOption: Option[Pid]): Unit = {
    autoClosing(new ProcessKillScriptProvider) { provider =>
      val tmp = createTempDirectory("test-")
      val killScript = provider.provideTo(temporaryDirectory)
      val args = killScript.toCommandArguments(agentTaskId, pidOption)
      val killProcess = new ProcessBuilder(args.asJava).startRobustly()
      startLogStreams(killProcess, "Kill script") await 60.s
      killProcess.waitFor(60, SECONDS)
      assert(killProcess.exitValue == 0)
      deleteDirectoryRecursively(tmp)
    }
  }
}

private object ProcessKillScriptTest {
  private val logger = Logger(getClass)
  private val TestAgentTaskId = AgentTaskId("1-TEST")
  private def Script =
    (if (isWindows) JavaResource("js7/taskserver/task/process/scripts/windows/test.cmd")
               else JavaResource("js7/taskserver/task/process/scripts/unix/test.sh"))
    .asUTF8String
  private val SIGKILLexitValue = if (isWindows) 1 else if (isSolaris) SIGKILL.value else 128 + SIGKILL.value

  private def logProcessTree(): Unit = {
    if (isUnix && !isMac) {
      val ps = new ProcessBuilder("ps", "fux").startRobustly()
      startLogStreams(ps, "ps (for information only, please ignore errors)") await 15.s
      ps.waitFor()
    }
  }

  private def startLogStreams(process: Process, prefix: String): Future[Any] =
    Future.sequence(List(
      Future[Unit] { blocking { logStream(process.getInputStream, s"$prefix stdout") }},
      Future[Unit] { blocking { logStream(process.getErrorStream, s"$prefix stderr") }}
    ))

  private def logStream(in: InputStream, prefix: String): Unit =
    logger.info("\n" + readLines(in, prefix).mkString("\n"))

  private def readLines(in: InputStream, prefix: String): Iterator[String] =
    for (line <- scala.io.Source.fromInputStream(in).getLines()) yield s"$prefix: $line"
}
