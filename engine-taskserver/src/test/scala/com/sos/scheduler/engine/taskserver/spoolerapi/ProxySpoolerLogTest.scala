package com.sos.scheduler.engine.taskserver.spoolerapi

import com.google.inject.Guice
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch._
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRemoting
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProxySpoolerLogTest extends FreeSpec {

  private lazy val injector = Guice.createInjector(new ScalaAbstractModule {
    def configure() = bindInstance[TaskStartArguments](TaskStartArguments.forTest())
  })

  for (minimumLevel ← List(SchedulerLogLevel.debug9, SchedulerLogLevel.error)) {
    testLog(minimumLevel, SchedulerLogLevel.debug1, "debug") { _.invokeMethod("debug", List("debug")) }
    for (level ← SchedulerLogLevel.values; if level != SchedulerLogLevel.none) {
      testLog(minimumLevel, level, level.cppName) { _.invokeMethod(level.cppName, List(level.cppName)) }
    }
  }

  /** Assures that the proxy method, called via `@invocable`, suppresses the message accordingly to the minimum level. */
  private def testLog(minimumLevel: SchedulerLogLevel, level: SchedulerLogLevel, message: String)(body: IDispatch ⇒ Unit): Unit = {
    val suppressed = level.cppNumber < minimumLevel.cppNumber
    s"$message at minimum $minimumLevel${if (suppressed) " is suppressed" else ""}" in {
      val remoting = mock[ProxyRemoting]
      val proxyId = ProxyId(Random.nextLong())
      val spoolerLog = ProxySpoolerLog(remoting, proxyId, name = "TEST", properties = List("level" → minimumLevel.cppNumber))
      body(spoolerLog)
      if (!suppressed) {
        verify(remoting).invoke(proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), arguments = List(level.cppNumber, message), namedArguments = Nil)
      }
      verifyNoMoreInteractions(remoting)
    }
  }

  "level" in {
    val remoting = mock[ProxyRemoting]
    val proxyId = ProxyId(Random.nextLong())

    val spoolerLog = ProxySpoolerLog(remoting, proxyId, name = "TEST", properties = List("level" → SchedulerLogLevel.warning.cppNumber))
    spoolerLog.invokeMethod("info", List("INFO"))   // Suppressed
    spoolerLog.invokeMethod("warn", List("WARNING"))
    verify(remoting).invoke(proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), arguments = List(SchedulerLogLevel.warning.cppNumber, "WARNING"), namedArguments = Nil)
    verifyNoMoreInteractions(remoting)

    reset(remoting)
    spoolerLog.invokePut("level", SchedulerLogLevel.info.cppNumber)
    spoolerLog.invokeMethod("debug9", List("DEBUG9"))  // Suppressed
    spoolerLog.invokeMethod("info", List("INFO"))
    verify(remoting).invoke(proxyId, ProxySpoolerLog.LevelDispid, Set(DISPATCH_PROPERTYPUT), arguments = Nil, namedArguments = List(DISPID.PROPERTYPUT → SchedulerLogLevel.info.cppNumber))
    verify(remoting).invoke(proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), arguments = List(SchedulerLogLevel.info.cppNumber, "INFO"), namedArguments = Nil)
    verifyNoMoreInteractions(remoting)
  }
}
