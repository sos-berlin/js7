package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.ShellProcessTask._
import com.sos.scheduler.engine.taskserver.task.common.VariableSets
import com.sos.scheduler.engine.taskserver.task.process.{ShellProcess, ShellProcessStarter}
import java.nio.charset.StandardCharsets._
import java.nio.file.Files._
import org.scalactic.Requirements._
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final class ShellProcessTask(
  module: ShellModule,
  namedInvocables: NamedInvocables,
  monitors: immutable.Seq[Monitor],
  jobName: String,
  hasOrder: Boolean,
  environment: Map[String, String]) 
extends Task with HasCloser {
  
  import namedInvocables.{spoolerLog, spoolerTask}

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private lazy val orderParamsFile = createTempFile("sos-", ".tmp")
  private var startCalled = false
  private var shellProcess: ShellProcess = null

  def start() = {
    startCalled = true
    monitorProcessor.preTask() && {
      if (monitorProcessor.preStep()) {
        shellProcess = startProcess()
      }
      true
    }
  }

  private def startProcess(): ShellProcess = {
    val env = {
      val params = spoolerTask.parameterMap ++ spoolerTask.orderParameterMap
      val paramEnv = params map { case (k, v) ⇒ s"$EnvironmentParameterPrefix$k" → v }
      environment + (ReturnValuesFileEnvironmentVariableName → orderParamsFile.toAbsolutePath.toString) ++ paramEnv
    }
    ShellProcessStarter.start(name = jobName, additionalEnvironment = env, scriptString = module.script.string.trim).closeWithCloser
  }

  def end() =
    if (startCalled) {
      monitorProcessor.postTask()
    }

  def step() = {
    requireState(startCalled)
    if (shellProcess == null)
      <process.result spooler_process_result="false"/>.toString()
    else {
      val rc = shellProcess.waitForTermination(logOutputLine = spoolerLog.info)
      val success = monitorProcessor.postStep(rc.isSuccess)
      transferReturnValuesToMaster()
      <process.result spooler_process_result={success.toString} exit_code={rc.value.toString} state_text={shellProcess.firstStdoutLine}/>.toString()
    }
  }

  private def transferReturnValuesToMaster(): Unit = {
    val variables = fetchReturnValues()
    if (variables.nonEmpty) {
      val xmlString = VariableSets.toXmlElem(fetchReturnValues()).toString()
      if (hasOrder)
        spoolerTask.orderParamsXml = xmlString
      else
        spoolerTask.paramsXml = xmlString
    }
  }

  private def fetchReturnValues() =
    autoClosing(io.Source.fromFile(orderParamsFile)(ReturnValuesFileEncoding)) { source ⇒
      (source.getLines map lineToKeyValue).toMap
    }

  def files = {
    requireState(startCalled)
    shellProcess match {
      case null ⇒ Nil
      case o ⇒ o.files
    }
  }
}

object ShellProcessTask {
  private val EnvironmentParameterPrefix = "SCHEDULER_PARAM_"
  private val ReturnValuesFileEnvironmentVariableName = "SCHEDULER_RETURN_VALUES"
  private val ReturnValuesFileEncoding = ISO_8859_1  // For v1.9 (and later ???)
  private val ReturnValuesRegex = "([^=]+)=(.*)".r

  private def lineToKeyValue(line: String): (String, String) = line match {
    case ReturnValuesRegex(name, value) ⇒ name.trim → value.trim
    case _ ⇒ throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in file denoted by environment variable $ReturnValuesFileEnvironmentVariableName: $line")
  }
}
