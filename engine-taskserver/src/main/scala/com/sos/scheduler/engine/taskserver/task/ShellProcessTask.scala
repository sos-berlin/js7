package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger, SetOnce}
import com.sos.scheduler.engine.common.utils.JavaShutdownHook
import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.taskserver.data.TaskServerConfiguration._
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.ShellProcessTask._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.taskserver.task.process.{ProcessConfiguration, RichProcess}
import java.nio.file.Files._
import java.nio.file.Path
import org.jetbrains.annotations.TestOnly
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

/**
 * @author Joacim Zschimmer
 *
 * @see spooler_module_process.cxx, C++ class Process_module_instance
 */
private[task] final class ShellProcessTask(
  module: ShellModule,
  protected val commonArguments: CommonArguments,
  environment: immutable.Iterable[(String, String)],
  variablePrefix: String,
  killScriptPathOption: Option[Path],
  taskServerMainTerminatedOption: Option[Future[Unit]] = None)
extends HasCloser with Task {

  import commonArguments.{agentTaskId, hasOrder, jobName, monitors, namedInvocables, stdFiles}
  import namedInvocables.spoolerTask

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private lazy val orderParamsFile = createTempFile("sos-", ".tmp")
  private lazy val processStdFileMap = if (stdFiles.isEmpty) RichProcess.createTemporaryStdFiles() else Map[StdoutStderrType, Path]()
  private lazy val concurrentStdoutStderrWell = new ConcurrentStdoutAndStderrWell(s"Job $jobName",
    stdFiles.copy(stdFileMap = processStdFileMap ++ stdFiles.stdFileMap)).closeWithCloser
  private var startCalled = false
  private val richProcessOnce = new SetOnce[RichProcess]
  private val logger = Logger.withPrefix(getClass, toString)
  private var sigtermForwarder: JavaShutdownHook = null

  def start() = {
    requireState(!startCalled)
    startCalled = true
    monitorProcessor.preTask() &&
      monitorProcessor.preStep() && {
        startProcess()
        true
      }
  }

  private def startProcess() = {
    for (terminated ← taskServerMainTerminatedOption) {
      sigtermForwarder = JavaShutdownHook.add(ShellProcessTask.getClass.getName) {
        sendProcessSignal(SIGTERM)
        Await.ready(terminated, Inf)  // Delay until TaskServer has been terminated
      }
    }
    val env = {
      val params = spoolerTask.parameterMap ++ spoolerTask.orderParameterMap
      val paramEnv = params map { case (k, v) ⇒ (variablePrefix concat k.toUpperCase) → v }
      environment ++ List(ReturnValuesFileEnvironmentVariableName → orderParamsFile.toAbsolutePath.toString) ++ paramEnv
    }
    val (idStringOption, killScriptFileOption) =
      if (taskServerMainTerminatedOption.nonEmpty) (None, None)
      else (Some(agentTaskId.string), killScriptPathOption)  // No idString if this is an own process (due to a monitor), already started with idString
    richProcessOnce := RichProcess.startShellScript(
      ProcessConfiguration(
        processStdFileMap,
        additionalEnvironment = env,
        idStringOption = idStringOption,
        killScriptFileOption = killScriptFileOption),
      name = jobName,
      scriptString = module.script.string.trim)
    .closeWithCloser
    deleteFilesWhenProcessClosed(List(orderParamsFile) ++ processStdFileMap.values)
    concurrentStdoutStderrWell.start()
  }

  private def deleteFilesWhenProcessClosed(files: Iterable[Path]): Unit = {
    if (files.nonEmpty) {
      for (richProcess ← richProcessOnce;
           _ ← richProcess.closed;
           _ ← concurrentStdoutStderrWell.closed)
      {
        RichProcess.tryDeleteFiles(files)
      }
    }
  }

  def end() = {}  // Not called

  def step() = {
    requireState(startCalled)
    richProcessOnce.get match {
      case None ⇒
        <process.result spooler_process_result="false"/>.toString()
      case Some(richProcess) ⇒
        val rc = richProcess.waitForTermination()
        if (sigtermForwarder != null) sigtermForwarder.close()
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
    autoClosing(io.Source.fromFile(orderParamsFile)(Encoding)) { source ⇒
      (source.getLines map lineToKeyValue).toMap
    }

  def sendProcessSignal(signal: ProcessSignal): Unit = {
    logger.trace(s"sendProcessSignal $signal")
    for (p ← richProcessOnce) p.sendProcessSignal(signal)
  }

  @TestOnly
  def files = {
    requireState(startCalled)
    richProcessOnce.toOption match {
      case None ⇒ Nil
      case Some(o) ⇒ o.processConfiguration.files
    }
  }

  override def toString = List(super.toString) ++ (richProcessOnce map { _.toString }) mkString " "

  def pidOption = richProcessOnce flatMap { _.pidOption }
}

private object ShellProcessTask {
  private val ReturnValuesFileEnvironmentVariableName = "SCHEDULER_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r

  private def lineToKeyValue(line: String): (String, String) = line match {
    case ReturnValuesRegex(name, value) ⇒ name.trim → value.trim
    case _ ⇒ throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in file denoted by environment variable $ReturnValuesFileEnvironmentVariableName: $line")
  }
}
