package com.sos.jobscheduler.taskserver.modules.shell

import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal._
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger, SetOnce}
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.common.xml.VariableSets
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.system.StdoutOrStderr
import com.sos.jobscheduler.taskserver.common.StdoutStderrWell
import com.sos.jobscheduler.taskserver.data.TaskServerConfiguration._
import com.sos.jobscheduler.taskserver.data.TaskServerMainTerminated
import com.sos.jobscheduler.taskserver.modules.common.{CommonArguments, Task}
import com.sos.jobscheduler.taskserver.modules.monitor.MonitorProcessor
import com.sos.jobscheduler.taskserver.modules.shell.ShellProcessTask._
import com.sos.jobscheduler.taskserver.task.process.ShellScriptProcess.startShellScript
import com.sos.jobscheduler.taskserver.task.process.{ProcessConfiguration, RichProcess}
import java.nio.file.Files._
import java.nio.file.Path
import org.jetbrains.annotations.TestOnly
import org.scalactic.Requirements._
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.io

/**
 * @author Joacim Zschimmer
  * @see spooler_module_process.cxx, C++ class Process_module_instance
 */
private[taskserver] final class ShellProcessTask(
  module: ShellModule,
  protected val commonArguments: CommonArguments,
  environment: Map[String, String],
  variablePrefix: String,
  logDirectory: Path,
  logFilenamePart: String,
  killScriptOption: Option[ProcessKillScript],
  synchronizedStartProcess: RichProcessStartSynchronizer,
  taskServerMainTerminatedOption: Option[Future[TaskServerMainTerminated.type]] = None)
  (implicit executionContext: ExecutionContext)
extends HasCloser with Task {

  import commonArguments.{agentTaskId, hasOrder, jobName, monitors, namedIDispatches, stdFiles}
  import namedIDispatches.spoolerTask

  private val monitorProcessor = MonitorProcessor.create(monitors, namedIDispatches).closeWithCloser
  private lazy val orderParamsFile = createTempFile("sos-", ".tmp")
  private lazy val processStdFileMap = if (stdFiles.isEmpty) RichProcess.createStdFiles(logDirectory, id = logFilenamePart) else Map[StdoutOrStderr, Path]()
  private var startCalled = false
  private val richProcessOnce = new SetOnce[RichProcess]
  private val logger = Logger.withPrefix[ShellProcessTask](toString)
  private var sigtermForwarder: Option[JavaShutdownHook] = None

  def start() = {
    requireState(!startCalled)
    startCalled = true
    val precondition = monitorProcessor.preTask() && {
      onClose {
        monitorProcessor.postTask()
      }
      monitorProcessor.preStep()
    }
    precondition &&
      startProcess()
  }

  private def startProcess() = {
    sigtermForwarder = for (terminated ← taskServerMainTerminatedOption) yield
      JavaShutdownHook.add(ShellProcessTask.getClass.getName) {
        sendProcessSignal(SIGTERM)
        terminated.awaitInfinite  // Delay until TaskServer has been terminated
        Log4j.shutdown()
      }
    val env = {
      val params = spoolerTask.parameterMap ++ spoolerTask.orderParameterMap
      val paramEnv = params map { case (k, v) ⇒ (variablePrefix concat k.toUpperCase) → v }
      environment + (ReturnValuesFileEnvironmentVariableName → orderParamsFile.toAbsolutePath.toString) ++ paramEnv
    }
    val (agentTaskIdOption, killScriptFileOption) =
      if (taskServerMainTerminatedOption.nonEmpty) (None, None)
      else (Some(agentTaskId), killScriptOption)  // No idString if this is an own process (due to a monitor), already started with idString
    val processConfiguration = ProcessConfiguration(
      processStdFileMap,
      encoding = Encoding,
      additionalEnvironment = env,
      agentTaskIdOption = agentTaskIdOption,
      killScriptOption = killScriptFileOption)
    richProcessOnce := synchronizedStartProcess {
      startShellScript(processConfiguration, name = jobName, scriptString = module.script.string.trim).closeWithCloser
    } .awaitInfinite
    deleteFilesWhenProcessClosed(List(orderParamsFile))
    true
  }

  def end() = {}  // Not called

  def step() = {
    requireState(startCalled)
    richProcessOnce.toOption match {
      case None ⇒
        logger.warn("step, but no process has been started")
        <process.result spooler_process_result="false" exit_code="999888999"/>.toString()
      case Some(richProcess) ⇒
        val (returnCode, firstStdoutLine) =
          withCloser { implicit closer ⇒
            val well = new StdoutStderrWell(processStdFileMap ++ stdFiles.stdFileMap, stdFiles.encoding, batchThreshold = Task.LogBatchThreshold, stdFiles.output)
              .closeWithCloser
            @tailrec def loop(): ReturnCode = {
              richProcess.waitForTermination(Task.LogPollPeriod) match {
                case None ⇒ well.apply(); loop()
                case Some(o) ⇒ o
              }
            }
            val returnCode = loop()
            for (o ← sigtermForwarder) o.close()
            transferReturnValuesToMaster()
            well.apply()
            (returnCode, well.firstStdoutLine)
          }
        val success = monitorProcessor.postStep(returnCode.isSuccess)
        <process.result spooler_process_result={success.toString} exit_code={returnCode.number.toString} state_text={firstStdoutLine}/>.toString()
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
    autoClosing(scala.io.Source.fromFile(orderParamsFile)(Encoding)) { source ⇒
      (source.getLines map lineToKeyValue).toMap
    }

  def sendProcessSignal(signal: ProcessSignal): Unit = {
    logger.trace(s"sendProcessSignal $signal")
    for (p ← richProcessOnce) p.sendProcessSignal(signal)
  }

  def deleteLogFiles() = deleteFilesWhenProcessClosed(processStdFileMap.values)

  private def deleteFilesWhenProcessClosed(files: Iterable[Path]): Unit = {
    if (files.nonEmpty) {
      for (richProcess ← richProcessOnce;
           _ ← richProcess.closed)
      {
        RichProcess.tryDeleteFiles(files)
      }
    }
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
    case _ ⇒ throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in files denoted by environment variable $ReturnValuesFileEnvironmentVariableName: $line")
  }
}
