package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.scheduler.job.task.ModuleInstanceRunner._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.stringToSource
import com.sos.jobscheduler.data.job.{ReturnCode, TaskId}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.minicom.remoting.proxy.ProxyIDispatch
import com.sos.jobscheduler.taskserver.task.TaskArguments
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class ModuleInstanceRunner(jobConfiguration: JobConfiguration, taskId: TaskId, moduleInstance: ProxyIDispatch)
  (implicit ec: ExecutionContext) {

  private val spoolerLog = new SpoolerLogIDispatch
  private val spoolerTask = new SpoolerTaskIDispatch(initialTaskVariables = jobConfiguration.variables)

  def start(): Future[Boolean] =
    for (() ← constructModuleInstance(moduleInstance);
         ok ← beginModuleInstance(moduleInstance))
      yield ok

  private def constructModuleInstance(moduleInstance: ProxyIDispatch): Future[Unit] =
    moduleInstance.asyncCall("construct", Vector(
      TaskArguments.toStrings(Vector(
        TaskArguments.JobKey → jobConfiguration.path.withoutStartingSlash,
        TaskArguments.TaskIdKey → taskId.string,
        TaskArguments.HasOrderKey → "1",
        TaskArguments.EnvironmentKey → "<variables/>",
        TaskArguments.module.LanguageKey → jobConfiguration.language,
        TaskArguments.module.ScriptKey → jobConfiguration.script.toXmlElem.toString()))))
    .mapTo[Unit]

  private def beginModuleInstance(moduleInstance: ProxyIDispatch): Future[Boolean] =
     moduleInstance.asyncCall("begin", Vector(
       Vector(spoolerLog, spoolerTask),
       Vector("spooler_log", "spooler_task")))
     .mapTo[Boolean]

  def processOrder(order: Order[Order.InProcess]): Future[TaskStepEnded] = {
    val orderIDispatch = new OrderIDispatch(order.variables)
    spoolerTask.order = orderIDispatch
    moduleInstance.asyncCall("step", Nil)
      .map { stepResult ⇒
        TaskStepSucceeded(
          MapDiff.diff(order.variables, orderIDispatch.variables),
          Outcome.Good(StepResult.fromXml(stepResult.asInstanceOf[String]).returnCode))
      }
      .recover {
        case t: Throwable ⇒ TaskStepFailed(Outcome.Bad(t.toString))  // We are exposing the exception message !!!
      }
  }

  def terminate(): Future[Completed] = {
    // Not for shell: moduleInstance.call("end", Nil)
    moduleInstance.release()
  }
}

object ModuleInstanceRunner {
  private case class StepResult(result: Boolean, returnCode: ReturnCode, stateText: String)

  private object StepResult {
    def fromXml(xmlString: String): StepResult =
      ScalaXMLEventReader.parseDocument(stringToSource(xmlString), config = ScalaXMLEventReader.Config(ignoreUnknown = true)) { eventReader ⇒
        import eventReader._
        parseElement("process.result") {
          StepResult(
            result = attributeMap.as[Boolean]("spooler_process_result"),
            returnCode = attributeMap.as[ReturnCode]("exit_code"),
            stateText = attributeMap.getOrElse("state_text", ""))
        }
      }
  }
}
