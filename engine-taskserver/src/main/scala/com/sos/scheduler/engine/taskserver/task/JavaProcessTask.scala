package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
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

  // FIXME Implemented incompletely, only as needed to test JS-1301.

  def start() = {
    //??? monitorProcessor.preTask()
    //??? instance.spooler_init()
    true
  }

  def callIfExists(methodWithSignature: String) = {
    val NameAndSignature(name, "", resultType) = methodWithSignature
    instance.getClass.getMethod(name).invoke(instance)
  }

  def step() = instance.spooler_process()

  def end() = {
//    instance.spooler_close()
//    instance.spooler_exit()
  }
}

object JavaProcessTask {
  private val NameAndSignature = """(.*)\((.*)\)(.+)""".r
  private val logger = Logger(getClass)
}
