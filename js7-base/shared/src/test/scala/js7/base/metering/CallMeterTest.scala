package js7.base.metering

import cats.effect.IO
import cats.syntax.traverse.*
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.log.Logger
import js7.base.metering.CallMeterTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.foreachWithBracket
import scala.jdk.CollectionConverters.*

final class CallMeterTest extends OurAsyncTestSuite:

  "Call" in:
    val n = 1_000_000

    locally:
      var i = n
      while i > 0 do
        meterConstant:
          1
        i -= 1

    locally:
      var i = n
      while i > 0 do
        meterDecrement:
          i -= 1

    assert(meterConstant.total == n)
    assert(meterConstant.measurement().duration > 0.s)
    CallMeter.logAll()
    succeed

  "IO" in:
    val n = 10_000

    (1 to n).toVector.traverse: _ =>
      meterIO:
        IO:
          Thread.sleep(0, 1000) // 1µs
    .productR:
      IO:
        assert(meterIO.measurement().total == n)
        assert(meterIO.measurement().duration > 1.µs * n)
        CallMeter.logAll()
        succeed

  "nanoTime duration only" in:
    val n = 1_000_000
    var i = n
    while i > 0 do
      meterNanoTime:
        System.nanoTime()
      i -= 1
    CallMeter.logAll()

    i = n
    val t = System.nanoTime()
    while i > 0 do
      System.nanoTime()
      i -= 1
    Logger.trace(itemsPerSecondString((System.nanoTime() - t).ns, n, "nanoTimes"))
    succeed

  "MXBeans" in:
    val objectName = new ObjectName("js7:name=*")
    val beanServer = ManagementFactory.getPlatformMBeanServer
    beanServer.queryNames(new ObjectName("js7:name=*"), null).asScala
      .foreachWithBracket(): (o, br) =>
        logger.info(s"$br$o")
    assert(beanServer.getDomains contains "js7")
    assert:
      beanServer.queryNames(new ObjectName("js7:name=*"), null).asScala.exists:
        _.getCanonicalName == "js7:name=CallMeter_CallMeterTest.nanoTime"
    assert:
      beanServer.queryNames(new ObjectName("js7:name=CallMeter_CallMeterTest.nanoTime"), null)
        .asScala.exists:
          _.getCanonicalName == "js7:name=CallMeter_CallMeterTest.nanoTime"


object CallMeterTest:
  private val logger = Logger[this.type]
  private val meterConstant = CallMeter()
  private val meterDecrement = CallMeter()
  private val meterIO = CallMeter()
  private val meterNanoTime = CallMeter("CallMeterTest.nanoTime")
