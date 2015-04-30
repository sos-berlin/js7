package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.taskserver.module.Module._
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.task.JavaProcessTask._
import scala.collection.immutable

/**
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

  def start() = {
    // TODO Monitor calls: monitorProcessor.preTask()
    require(monitorProcessor.isEmpty, "Monitors for Java jobs are not yet implemented")
    instance.spooler_init()
  }

  def callIfExists(methodWithSignature: String) = {
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
  }

  private def ignoreCall(methodWithSignature: String): Boolean = {
    // As in C++ Module_instance::call__end
    methodWithSignature match {
      case SpoolerOnSuccessSignature if !openCalled ⇒ true
      case SpoolerOnErrorSignature if !openCalled ⇒ true
      case SpoolerExitSignature if exitCalled ⇒ true
      case _ ⇒ false
    }
  }

  def step() = instance.spooler_process()

  def end() = {
    if (openCalled && !closeCalled) {
      closeCalled = true
      instance.spooler_close()
    }
  }
}

object JavaProcessTask {
  private val NameAndSignature = """(.*)\((.*)\)(.+)""".r
  private val logger = Logger(getClass)
}
