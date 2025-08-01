package js7.base.metering

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.metering.CallMeterTest.{meterConstant, meterDecrement, meterIO, meterNanoTime}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString

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


object CallMeterTest:
  private val meterConstant = CallMeter()
  private val meterDecrement = CallMeter()
  private val meterIO = CallMeter()
  private val meterNanoTime = CallMeter()
