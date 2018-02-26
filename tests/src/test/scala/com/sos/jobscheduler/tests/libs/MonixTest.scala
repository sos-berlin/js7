package com.sos.jobscheduler.tests.libs

import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import monix.reactive._
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object MonixTest extends FreeSpec
{
  def main(args: Array[String]) = {
    val tick = Observable.interval(1.second)
      .filter(_ % 2 == 0)
      .map(_ * 10)
      .flatMap(x ⇒ Observable.fromIterable(Seq(x, x)))
      .take(5)
      .dump("Out")
    val cancelable = tick.subscribe()
    sleep(3.s)
    cancelable.cancel()
  }
}
