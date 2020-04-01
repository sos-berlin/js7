package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.base.process.ProcessSignal.SIGKILL
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.process.Processes
import com.sos.jobscheduler.common.process.Processes.{Pid, RobustlyStartProcess, processToPidOption}
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.FileUtils._
import com.sos.jobscheduler.common.system.OperatingSystem.{isMac, isSolaris, isUnix, isWindows}
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.taskserver.task.process.ProcessKillScriptTest._
import java.io.InputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files._
import java.nio.file.Path
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends FreeSpec {

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
    val args = List(file.toString, s"-agent-task-id=${agentTaskId.string}")
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
    (if (isWindows) JavaResource("com/sos/jobscheduler/taskserver/task/process/scripts/windows/test.cmd")
               else JavaResource("com/sos/jobscheduler/taskserver/task/process/scripts/unix/test.sh"))
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
