package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGKILL
import com.sos.scheduler.engine.common.process.Processes
import com.sos.scheduler.engine.common.process.Processes.{Pid, processToPidOption}
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.{isSolaris, isUnix, isWindows}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptTest._
import java.io.InputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files._
import java.nio.file.Path
import java.util.concurrent.TimeUnit.SECONDS
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.io

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ProcessKillScriptTest extends FreeSpec {

  "Kill script kills descendants" in {
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
    logger.info("All processes should been killed now")
    logProcessTree()
    val grown = out.contentString stripPrefix beforeKill
    assert(grown == "", "Stdout file must not grow after kill script")
    delete(scriptFile)
    delete(out)
  }

  private def startNestedProcess(agentTaskId: AgentTaskId, out: Path) = {
    val file = Processes.newTemporaryShellFile("test")
    file.contentString = Script
    val args = List(file.toString, s"-agent-task-id=${agentTaskId.string}")
    val process = new ProcessBuilder(args).redirectOutput(out).redirectError(INHERIT).start()
    logger.info(s"Started process ${processToPidOption(process)}")
    (file, process)
  }

  private def runKillScript(agentTaskId: AgentTaskId, pidOption: Option[Pid]): Unit = {
    autoClosing(new ProcessKillScriptProvider) { provider ⇒
      val killScript = provider.provideTo(temporaryDirectory)
      val args = killScript.toCommandArguments(agentTaskId, pidOption)
      val killProcess = new ProcessBuilder(args).start()
      startLogStreams(killProcess, "Kill script") await 60.s
      killProcess.waitFor(60, SECONDS)
      assert(killProcess.exitValue == 0)
    }
  }
}

private object ProcessKillScriptTest {
  private val logger = Logger(getClass)
  private val TestAgentTaskId = AgentTaskId("1-TEST")
  private def Script =
    (if (isWindows) JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/windows/test.cmd")
               else JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/unix/test.sh"))
    .asUTF8String
  private val SIGKILLexitValue = if (isWindows) 1 else if (isSolaris) SIGKILL.value else 128 + SIGKILL.value

  private def logProcessTree(): Unit = {
    if (isUnix) {
      val ps = new ProcessBuilder(List("ps", "fux")).start()
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
    for (line ← io.Source.fromInputStream(in).getLines()) yield s"$prefix: $line"
}
