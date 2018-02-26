package com.sos.jobscheduler.tests.libs

import cats.implicits._
import cats.data._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class CatsTest extends FreeSpec
{
  "OptionT" in {
    val aFuture = Future { 3.some }
    val bFuture = Future { 7.some }
    val c: OptionT[Future, Int] = for {
      a ← OptionT(aFuture)
      b ← OptionT(bFuture)
    } yield a * b
    assert(c.value.await(99.s) == 21.some)
  }
}
