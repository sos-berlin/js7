package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.taskserver.module.Module._
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.task.JavaProcessTask._
import scala.collection.immutable
import scala.util.control.NonFatal

/**
 * Runs a Java job, calling the job's method spooler_open, spooler_process etc.
 * Behaves as C++ Module_instance (spooler_module.cxx).
 *
 * @author Joacim Zschimmer
 */
final class JavaProcessTask(
  module: JavaModule,
  namedInvocables: NamedInvocables,
  monitors: immutable.Seq[Monitor],
  jobName: String,
  hasOrder: Boolean,
  environment: immutable.Iterable[(String, String)])
extends Task with HasCloser {

  private val monitorProcessor = new MonitorProcessor(monitors, namedInvocables, jobName = jobName).closeWithCloser
  private val instance: sos.spooler.Job_impl = module.newJobInstance(namedInvocables)
  private var openCalled = false
  private var closeCalled = false
  private var exitCalled = false

  def start() = monitorProcessor.preTask() && instance.spooler_init()

  /**
   * Behaves as Module_instance::call&#95;&#95;end.
   * Method must exists. Job must implement [[sos.spooler.Job_impl]].
   */
  def callIfExists(methodWithSignature: String) =
    if (ignoreCall(methodWithSignature)) {
      logger.debug(s"Call ignored: $methodWithSignature")
      ()
    } else {
      methodWithSignature match {
        case SpoolerOpenSignature ⇒ openCalled = true
        case SpoolerExitSignature ⇒ exitCalled = true
        case _ ⇒
      }
      val NameAndSignature(name, "", _) = methodWithSignature
      instance.getClass.getMethod(name).invoke(instance)
    }

  /** Behaves as C++ Module_instance::call&#95;&#95;end. */
  private def ignoreCall(methodWithSignature: String): Boolean =
    methodWithSignature match {
      case SpoolerOnSuccessSignature if !openCalled ⇒ true
      case SpoolerOnErrorSignature if !openCalled ⇒ true
      case SpoolerExitSignature if exitCalled ⇒ true
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
            namedInvocables.spoolerTask.setErrorCodeAndText(StandardJavaErrorCode, s"$StandardJavaErrorCode  $t")  // Without Z-JAVA-105 description "Java exception $1, method=$2"
            false
        }
        monitorProcessor.postStep(result)
      }

  /** Behaves as C++ Module_instance::end&#95;&#95;end. */
  def end() = {
    if (openCalled && !closeCalled) {
      closeCalled = true
      instance.spooler_close()
    }
    monitorProcessor.postTask()
  }
}

object JavaProcessTask {
  private val NameAndSignature = """(.*)\((.*)\)(.+)""".r
  private val StandardJavaErrorCode = MessageCode("Z-JAVA-105")
  private val logger = Logger(getClass)
}
