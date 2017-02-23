package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch._
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRemoting
import com.sos.scheduler.engine.taskserver.spoolerapi.ProxySpoolerLogTest._
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerLogTest extends FreeSpec {

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
      val remoting = new TestProxyRemoting
      val proxyId = ProxyId(Random.nextLong())
      val spoolerLog = ProxySpoolerLog(remoting, proxyId, name = "TEST", properties = List("level" → minimumLevel.cppNumber))
      body(spoolerLog)
      assert(remoting._invokes == (
        if (suppressed)
          Nil
        else List(
          (proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), List(level.cppNumber, message), Nil))))
    }
  }

  "level" in {
    val remoting = new TestProxyRemoting
    val proxyId = ProxyId(Random.nextLong())

    val spoolerLog = ProxySpoolerLog(remoting, proxyId, name = "TEST", properties = List("level" → SchedulerLogLevel.warning.cppNumber))
    spoolerLog.invokeMethod("info", List("INFO"))   // Suppressed
    spoolerLog.invokeMethod("warn", List("WARNING"))
    assert(remoting._invokes == List(
      (proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), List(SchedulerLogLevel.warning.cppNumber, "WARNING"), Nil)))
    remoting._invokes.clear()
    spoolerLog.invokePut("level", SchedulerLogLevel.info.cppNumber)
    spoolerLog.invokeMethod("debug9", List("DEBUG9"))  // Suppressed
    spoolerLog.invokeMethod("info", List("INFO"))
    assert(remoting._invokes == List(
      (proxyId, ProxySpoolerLog.LevelDispid, Set(DISPATCH_PROPERTYPUT), Nil, List(DISPID.PROPERTYPUT → SchedulerLogLevel.info.cppNumber)),
      (proxyId, ProxySpoolerLog.LogDispid, Set(DISPATCH_METHOD), List(SchedulerLogLevel.info.cppNumber, "INFO"), Nil))                    )
  }
}

private object ProxySpoolerLogTest {
  private class TestProxyRemoting extends ProxyRemoting {
    val _invokes = mutable.Buffer[(ProxyId, DISPID, Set[DispatchType], Seq[Any], Seq[(DISPID, Any)])]()

    def release(proxyId: ProxyId): Future[Completed] =
      throw new NotImplementedError

    def call(proxyId: ProxyId, methodName: String, arguments: Seq[Any]) =
      throw new NotImplementedError

    def getIdOfName(proxyId: ProxyId, name: String) =
      throw new NotImplementedError

    def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
      _invokes += ((proxyId, dispId, dispatchTypes, arguments, namedArguments))
      Future.successful(())
    }
  }
}
