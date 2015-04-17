package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.ShellProcessTask._
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
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
  environment: immutable.Iterable[(String, String)])
extends Task with HasCloser {
  
  import namedInvocables.{spoolerLog, spoolerTask}

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private lazy val orderParamsFile = createTempFile("sos-", ".tmp")
  private var startCalled = false
  private var richProcess: RichProcess = null

  def start() = {
    requireState(!startCalled)
    startCalled = true
    monitorProcessor.preTask() &&
      monitorProcessor.preStep() && {
        richProcess = startProcess()
        true
      }
  }

  private def startProcess(): RichProcess = {
    val env = {
      val params = spoolerTask.parameterMap ++ spoolerTask.orderParameterMap
      val paramEnv = params map { case (k, v) ⇒ s"$EnvironmentParameterPrefix${k.toUpperCase()}" → v }
      environment ++ List(ReturnValuesFileEnvironmentVariableName → orderParamsFile.toAbsolutePath.toString) ++ paramEnv
    }
    RichProcess.startShellScript(name = jobName, additionalEnvironment = env, scriptString = module.script.string.trim).closeWithCloser
  }

  def end() =
    if (startCalled) {
      monitorProcessor.postTask()
    }

  def step() = {
    requireState(startCalled)
    if (richProcess == null)
      <process.result spooler_process_result="false"/>.toString()
    else {
      val rc = richProcess.waitForTermination(logOutputLine = spoolerLog.info)
      val success = monitorProcessor.postStep(rc.isSuccess)
      transferReturnValuesToMaster()
      <process.result spooler_process_result={success.toString} exit_code={rc.toInt.toString} state_text={richProcess.firstStdoutLine}/>.toString()
    }
  }

  def callIfExists(javaSignature: String) = {
    requireState(startCalled)
    logger.debug(s"Ignoring call $javaSignature")
    true
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
    richProcess match {
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
  private val logger = Logger(getClass)

  private def lineToKeyValue(line: String): (String, String) = line match {
    case ReturnValuesRegex(name, value) ⇒ name.trim → value.trim
    case _ ⇒ throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in file denoted by environment variable $ReturnValuesFileEnvironmentVariableName: $line")
  }
}
