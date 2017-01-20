package com.sos.scheduler.engine.taskserver.dotnet

import com.sos.scheduler.engine.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.utils.Exceptions.ignoreException
import com.sos.scheduler.engine.taskserver.dotnet.SimpleDotnetTest._
import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleReference, TaskContext}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
trait SimpleDotnetTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  protected def language: String

  private lazy val dotnetEnvironment = new DotnetEnvironment(temporaryDirectory) sideEffect { o ⇒
    onClose {
      ignoreException(logger.asLazy.debug) {
        o.close()
      }
    }
  }
  private lazy val instanceFactory = new Jni4netModuleInstanceFactory(dotnetEnvironment.directory).closeWithCloser

  override protected def afterAll() = closer.close()

  protected def addScriptErrorTest(dotnetModuleReference: DotnetModuleReference): Unit = {
    s"Error in $language script is detected" in {
      val spoolerLogInvoker, spoolerTaskInvoker, spoolerJobInvoker, spoolerInvoker, orderInvoker, variablesInvoker = mock[sos.spooler.Invoker]
      val order = new sos.spooler.Order(orderInvoker)
      val spoolerVariableSet = new sos.spooler.Variable_set(variablesInvoker)
      when(spoolerInvoker.call("<variables", Array())).thenReturn(spoolerVariableSet, spoolerVariableSet)
      when(variablesInvoker.call("<value", Array("scheduler.variable_name_prefix"))).thenReturn("SCHEDULER_PARAM_", "SCHEDULER_PARAM_")
      when(spoolerTaskInvoker.call("<order", Array())).thenReturn(order, null)

      val taskContext = TaskContext(
        new sos.spooler.Log(spoolerLogInvoker),
        new sos.spooler.Task(spoolerTaskInvoker),
        new sos.spooler.Job(spoolerJobInvoker),
        new sos.spooler.Spooler(spoolerInvoker))

      val job = instanceFactory.newInstance(classOf[sos.spooler.IJob_impl], taskContext, dotnetModuleReference)
      job.spooler_init()
      val e = intercept[Exception] {
        job.spooler_process()
      }
      // FIXME Implement method job.close()
      assert(e.toString contains TestErrorMessage)
    }
  }

  protected def addStandardTest(dotnetModuleReference: DotnetModuleReference): Unit = {
    s"$language calls spooler_log.info" in {
      val spoolerLogInvoker, spoolerTaskInvoker, spoolerJobInvoker, spoolerInvoker, orderInvoker, paramsInvoker, variablesInvoker = mock[sos.spooler.Invoker]
      val order = new sos.spooler.Order(orderInvoker)

      val spoolerVariableSet = new sos.spooler.Variable_set(variablesInvoker)
      val variableSet = new sos.spooler.Variable_set(paramsInvoker)
      when(spoolerInvoker.call("<variables", Array())).thenReturn(spoolerVariableSet, spoolerVariableSet)
      when(variablesInvoker.call("<value", Array("scheduler.variable_name_prefix"))).thenReturn("SCHEDULER_PARAM_", "SCHEDULER_PARAM_")
      when(spoolerTaskInvoker.call("<order", Array())).thenReturn(order, order)
      when(orderInvoker.call("<params", Array())).thenReturn(variableSet, null)
      when(paramsInvoker.call("<value", Array("TEST"))).thenReturn("HELLO", null)
      val taskContext = TaskContext(
        new sos.spooler.Log(spoolerLogInvoker),
        new sos.spooler.Task(spoolerTaskInvoker),
        new sos.spooler.Job(spoolerJobInvoker),
        new sos.spooler.Spooler(spoolerInvoker))
      val job = instanceFactory.newInstance(classOf[sos.spooler.IJob_impl], taskContext, dotnetModuleReference)
      job.spooler_init()
      val result = job.spooler_process()
      assert(result)
      // FIXME Implement method job.close()
      verify(spoolerTaskInvoker, times(1)).call("<order", Array())
      verify(orderInvoker).call("<params", Array())
      verify(paramsInvoker).call("<value", Array("TEST"))
      verify(paramsInvoker).call(">value", Array("TEST", "TEST-CHANGED"))
      verify(spoolerLogInvoker).call("log", Array(0: Integer, "HELLO"))
    }
  }
}

private[dotnet] object SimpleDotnetTest {
  private val logger = Logger(getClass)
  private[dotnet] val TestErrorMessage = "TEST-ERROR"
}
