package com.sos.scheduler.engine.data.order

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderStatisticsTest extends FreeSpec {

  "plus" in {
    assert(
      OrderStatistics(   1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,   13,   14,   15) +
      OrderStatistics(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 1000, 1100, 1200, 1313, 1414, 1515) ==
      OrderStatistics(1001, 2002, 3003, 4004, 5005, 6006, 7007, 8008, 9009, 1010, 1111, 1212, 1313, 1414, 1515))
  }

  "JSON" in {
    val o = OrderStatistics(
      total = 1,
      notPlanned = 2,
      notSuspendedNotPlanned = 3,
      planned = 4,
      due = 5,
      started = 6,
      inTask = 7,
      inProcess = 8,
      setback = 9,
      waitingForResource = 10,
      notSuspendedWaitingForResource = 11,
      suspended = 12,
      blacklisted = 13,
      permanent = 14,
      fileOrder = 15)
    val json = """{
        "total": 1,
        "notPlanned": 2,
        "notSuspendedNotPlanned": 3,
        "planned": 4,
        "due": 5,
        "started": 6,
        "inTask": 7,
        "inProcess": 8,
        "setback": 9,
        "waitingForResource": 10,
        "notSuspendedWaitingForResource": 11,
        "suspended": 12,
        "blacklisted": 13,
        "permanent": 14,
        "fileOrder": 15
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[OrderStatistics] == o)
  }

  "Mutable +=" in {
    val a = new OrderStatistics.Mutable
    a += OrderStatistics(   1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,   13,   14,   15)
    a += OrderStatistics(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 1000, 1100, 1200, 1300, 1400, 1500)
    assert(a.toImmutable ==
         OrderStatistics(1001, 2002, 3003, 4004, 5005, 6006, 7007, 8008, 9009, 1010, 1111, 1212, 1313, 1414, 1515))
  }
}
