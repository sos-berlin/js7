package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.taskserver.HasSendProcessSignal
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.ShellProcessTask._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.taskserver.task.process.{ProcessConfiguration, RichProcess}
import java.nio.charset.StandardCharsets._
import java.nio.file.Files._
import java.nio.file.Path
import org.jetbrains.annotations.TestOnly
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 *
 * @see spooler_module_process.cxx, C++ class Process_module_instance
 */
final class ShellProcessTask(
  agentTaskId: AgentTaskId,
  jobName: String,
  module: ShellModule,
  namedInvocables: NamedInvocables,
  monitors: immutable.Seq[Monitor],
  hasOrder: Boolean,
  stdFiles: StdFiles,
  environment: immutable.Iterable[(String, String)],
  killScriptPathOption: Option[Path])
extends HasCloser with Task with HasSendProcessSignal {
  
  import namedInvocables.spoolerTask

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private lazy val orderParamsFile = createTempFile("sos-", ".tmp")
  private lazy val processStdFileMap = if (stdFiles.isEmpty) RichProcess.createTemporaryStdFiles() else Map[StdoutStderrType, Path]()
  private lazy val concurrentStdoutStderrWell = new ConcurrentStdoutAndStderrWell(s"Job $jobName",
    stdFiles.copy(stdFileMap = processStdFileMap ++ stdFiles.stdFileMap)).closeWithCloser
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
      val paramEnv = params map { case (k, v) ⇒ paramNameToEnv(k) → v }
      environment ++ List(ReturnValuesFileEnvironmentVariableName → orderParamsFile.toAbsolutePath.toString) ++ paramEnv
    }
    val richProcess = RichProcess.startShellScript(
      ProcessConfiguration(
        processStdFileMap,
        additionalEnvironment = env,
        idStringOption = Some(agentTaskId.string),
        killScriptFileOption = killScriptPathOption),
      name = jobName,
      scriptString = module.script.string.trim)
    .closeWithCloser
    if (processStdFileMap.nonEmpty) {
      for (_ ← richProcess.closed; _ ← concurrentStdoutStderrWell.closed) RichProcess.tryDeleteFiles(processStdFileMap.values)
    }
    concurrentStdoutStderrWell.start()
    richProcess
  }

  def end() = {}  // Not called

  def step() = {
    requireState(startCalled)
    if (richProcess == null)
      <process.result spooler_process_result="false"/>.toString()
    else {
      val rc = richProcess.waitForTermination()
      concurrentStdoutStderrWell.finish()
      transferReturnValuesToMaster()
      val success =
        try monitorProcessor.postStep(rc.isSuccess)
        finally monitorProcessor.postTask()
      <process.result spooler_process_result={success.toString} exit_code={rc.toInt.toString} state_text={concurrentStdoutStderrWell.firstStdoutLine}/>.toString()
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

  def sendProcessSignal(signal: ProcessSignal) = {
    logger.trace(s"sendProcessSignal $signal")
    for (p ← Option(richProcess)) p.sendProcessSignal(signal)
  }

  @TestOnly
  def files = {
    requireState(startCalled)
    richProcess match {
      case null ⇒ Nil
      case o ⇒ o.processConfiguration.files
    }
  }
}

object ShellProcessTask {
  private val ReturnValuesFileEnvironmentVariableName = "SCHEDULER_RETURN_VALUES"
  private val ReturnValuesFileEncoding = ISO_8859_1
  private val ReturnValuesRegex = "([^=]+)=(.*)".r
  private val logger = Logger(getClass)

  private def paramNameToEnv(name: String) = s"SCHEDULER_PARAM_${name.toUpperCase}"

  private def lineToKeyValue(line: String): (String, String) = line match {
    case ReturnValuesRegex(name, value) ⇒ name.trim → value.trim
    case _ ⇒ throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in file denoted by environment variable $ReturnValuesFileEnvironmentVariableName: $line")
  }
}
