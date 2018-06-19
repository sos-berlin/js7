package com.sos.jobscheduler.common.akkahttp.web.session

import monix.execution.schedulers.TestScheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class HasTimeoutTest extends FreeSpec
{
  private val t = new HasTimeout {}

  "test" in {
    implicit val scheduler = TestScheduler()
    assert(t.isEternal)
    assert(t.isAlive)

    t.touch(1.hour)
    assert(!t.isEternal)
    assert(t.isAlive)
    scheduler.tick(1.minute)
    assert(t.isAlive)

    scheduler.tick(1.hour)
    assert(!t.isAlive)
  }
}
