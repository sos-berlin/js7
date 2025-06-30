package js7.base.time

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.SchemeRestriction.{MonthRestriction, Unrestricted}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.BitSet
import scala.concurrent.duration.DurationInt

final class RestrictedSchemeTest extends AnyFreeSpec:

  "JSON" in:
    testJson[RestrictedScheme](
      RestrictedScheme(
        Seq(DailyPeriod(3600, 1.minute)),
        MonthRestriction(Set(3, 9))),
      json"""{
        "periods": [
          {
            "TYPE": "DailyPeriod",
            "duration": 60,
            "secondOfDay": 3600
          }
        ],
        "restriction": {
          "TYPE": "MonthRestriction",
          "months": [ 3, 9 ]
        }
      }""")

    testJson[RestrictedScheme](
      RestrictedScheme(
        Seq(DailyPeriod(3600, 1.minute)),
        Unrestricted),
      json"""{
        "periods": [
          {
            "TYPE": "DailyPeriod",
            "duration": 60,
            "secondOfDay": 3600
          }
        ]
      }""")

  "MonthRestriction" in:
    assert:
      MonthRestriction(BitSet(1, 6, 12)).toString == "MonthRestriction(January June December)"
    assert:
      MonthRestriction(BitSet(2, 3, 4)).toString == "MonthRestriction(February March April)"
    assert:
      MonthRestriction(BitSet(5, 7, 8)).toString == "MonthRestriction(May July August)"
    assert:
      MonthRestriction(BitSet(9, 10, 11)).toString == "MonthRestriction(September October November)"
