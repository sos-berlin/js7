package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.data.jobapi.JavaJobSignatures.{SpoolerExitSignature, SpoolerOnErrorSignature, SpoolerOnSuccessSignature, SpoolerOpenSignature}
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.taskserver.module.javamodule.JavaModule
import com.sos.scheduler.engine.taskserver.task.JavaProcessTask._
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * Runs a Java job, calling the job's method spooler_open, spooler_process etc.
 * Behaves as C++ Module_instance (spooler_module.cxx).
 *
 * @author Joacim Zschimmer
 */
private[task] final class JavaProcessTask(module: JavaModule, protected val commonArguments: CommonArguments)
extends Task with HasCloser {

  import commonArguments.{jobName, monitors, namedInvocables, stdFiles}
  import namedInvocables.spoolerTask

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private val instance: sos.spooler.Job_impl = module.newJobInstance(namedInvocables)
  private val methodIsCalled = mutable.Set[String]()
  private val concurrentStdoutStderrWell = stdFiles.nonEmpty option new ConcurrentStdoutAndStderrWell(s"Job $jobName", stdFiles).closeWithCloser
  private var closeCalled = false
  private val logger = Logger.withPrefix(getClass, toString)

  def start() = {
    concurrentStdoutStderrWell foreach { _.start() }
    monitorProcessor.preTask() && instance.spooler_init()
  }

  /**
   * Behaves as Module_instance::call&#95;&#95;end.
   * Method must exists. Job must implement [[sos.spooler.Job_impl]].
   */
  def callIfExists(methodWithSignature: String) =
    if (ignoreCall(methodWithSignature)) {
      logger.debug(s"Call ignored: $methodWithSignature")
      ()
    } else {
      methodIsCalled += methodWithSignature
      val NameAndSignature(name, "", _) = methodWithSignature
      try instance.getClass.getMethod(name).invoke(instance)
      finally
        if (methodWithSignature == SpoolerExitSignature) {
          afterSpoolerExit()
        }
    }

  private def afterSpoolerExit(): Unit =
    try monitorProcessor.postTask()
    finally concurrentStdoutStderrWell foreach { _.finish() }

  private def ignoreCall(methodWithSignature: String): Boolean =
    methodWithSignature match {
      case SpoolerOnSuccessSignature if !methodIsCalled(SpoolerOpenSignature) ⇒ true
      case SpoolerOnErrorSignature if !methodIsCalled(SpoolerOpenSignature) ⇒ true
      case SpoolerExitSignature if methodIsCalled(SpoolerExitSignature) ⇒ true
      case _ ⇒ false
    }

  /** Behaves as C++ Module_instance::step&#95;&#95;end. */
  def step() =
    if (monitorProcessor.isEmpty)
      instance.spooler_process()
    else
      monitorProcessor.preStep() && {
        val result = try instance.spooler_process()
        catch {
          case NonFatal(t) ⇒
            spoolerTask.setErrorCodeAndText(StandardJavaErrorCode, s"$StandardJavaErrorCode  $t")  // Without Z-JAVA-105 description "Java exception $1, method=$2"
            false
        }
        monitorProcessor.postStep(result)
      }

  /** Behaves as C++ Module_instance::end&#95;&#95;end. */
  def end() = {
    if (methodIsCalled(SpoolerOpenSignature) && !closeCalled) {
      closeCalled = true
      instance.spooler_close()
    }
  }

  def pidOption = None
}

private object JavaProcessTask {
  private val NameAndSignature = """(.*)\((.*)\)(.+)""".r
  private val StandardJavaErrorCode = MessageCode("Z-JAVA-105")
}
