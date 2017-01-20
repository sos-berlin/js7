package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.data.jobapi.JavaJobSignatures.{SpoolerExitSignature, SpoolerOnErrorSignature, SpoolerOnSuccessSignature, SpoolerOpenSignature}
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.taskserver.common.ConcurrentStdoutAndStderrWell
import com.sos.scheduler.engine.taskserver.modules.common.{CommonArguments, Task}
import com.sos.scheduler.engine.taskserver.modules.javamodule.ApiProcessTask._
import com.sos.scheduler.engine.taskserver.modules.monitor.MonitorProcessor
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * Runs a Java job, calling the job's method spooler_open, spooler_process etc.
 * Behaves as C++ Module_instance (spooler_module.cxx).
 *
 * @author Joacim Zschimmer
 */
private[taskserver] final class ApiProcessTask(module: ApiModule, protected val commonArguments: CommonArguments)
extends Task with HasCloser {

  import commonArguments.{jobName, monitors, namedIDispatches, stdFiles}
  import namedIDispatches.spoolerTask

  private val monitorProcessor = MonitorProcessor.create(monitors, namedIDispatches).closeWithCloser
  private val instance: sos.spooler.IJob_impl = module.newJobInstance(namedIDispatches)
  private val methodIsCalled = mutable.Set[String]()
  private val concurrentStdoutStderrWell = stdFiles.nonEmpty option new ConcurrentStdoutAndStderrWell(s"Job $jobName", stdFiles).closeWithCloser
  private var closeCalled = false
  private val logger = Logger.withPrefix(getClass, toString)

  def start() = {
    concurrentStdoutStderrWell foreach { _.start() }
    val result = monitorProcessor.preTask() && instance.spooler_init()
    Future.successful(result)
  }

  /**
   * Behaves as Module_instance::call&#95;&#95;end.
   * Method must exists. Job must implement [[sos.spooler.IJob_impl]].
   */
  def callIfExists(methodWithSignature: String) =
    if (ignoreCall(methodWithSignature)) {
      logger.debug(s"Call ignored: $methodWithSignature")
      ()
    } else {
      methodIsCalled += methodWithSignature
      val NameAndSignature(name, "", _) = methodWithSignature
      try instance.getClass.getMethod(name).invoke(instance)
      finally if (methodWithSignature == SpoolerExitSignature) {
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
  def step() = {
    val postResult =
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
    for (o ← concurrentStdoutStderrWell) o.flush()
    postResult
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

private object ApiProcessTask {
  private val NameAndSignature = """(.*)\((.*)\)(.+)""".r
  private val StandardJavaErrorCode = MessageCode("Z-JAVA-105")
}
