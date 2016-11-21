package com.sos.scheduler.engine.data.order

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JocOrderStatisticsTest extends FreeSpec {

  "plus" in {
    assert(
      JocOrderStatistics(  -1,   2,   3,   4,   5,   6,   7,   8,   9,   10,   11,   12,   13,   14) +
      JocOrderStatistics(-100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400) ==
      JocOrderStatistics(-101, 202, 303, 404, 505, 606, 707, 808, 909, 1010, 1111, 1212, 1313, 1414))
  }

  "JSON" in {
    val o = JocOrderStatistics(
      total = -1,
      notPlanned = 2,
      planned = 3,
      due = 4,
      started = 5,
      inTask = 6,
      inTaskProcess = 7,
      occupiedByClusterMember = 8,
      setback = 9,
      waitingForResource = 10,
      suspended = 11,
      blacklisted = 12,
      permanent = 13,
      fileOrder = 14)
    val json = """{
        "total": -1,
        "notPlanned": 2,
        "planned": 3,
        "due": 4,
        "started": 5,
        "inTask": 6,
        "inTaskProcess": 7,
        "occupiedByClusterMember": 8,
        "setback": 9,
        "waitingForResource": 10,
        "suspended": 11,
        "blacklisted": 12,
        "permanent": 13,
        "fileOrder": 14
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[JocOrderStatistics] == o)
  }

  "Mutable +=" in {
    val a = new JocOrderStatistics.Mutable
    a += JocOrderStatistics(-   1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,   13,   14)
    a += JocOrderStatistics(-1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 1000, 1100, 1200, 1300, 1400)
    assert(a.toImmutable ==
         JocOrderStatistics(-1001, 2002, 3003, 4004, 5005, 6006, 7007, 8008, 9009, 1010, 1111, 1212, 1313, 1414))
  }
}
