package js7.base.time

import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.Problem
import js7.base.time.SchemeRestriction.{MonthRestriction, Unrestricted}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.BitSet
import scala.concurrent.duration.DurationInt

final class RestrictedSchemeTest extends AnyFreeSpec:

  "JSON" - {
    "RestrictedScheme" in:
      testJson[RestrictedScheme](
        RestrictedScheme(
          Seq(DailyPeriod(3600, 1.minute)),
          SchemeRestriction.months(Set(3, 9)).orThrow),
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
            "months": [3, 9]
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

    "SchemeRestriction" in:
      testJson[SchemeRestriction](
        SchemeRestriction.months(Set(1, 2, 3, 12)).orThrow,
        json"""{
          "TYPE": "MonthRestriction",
          "months": [1, 2, 3, 12]
        }""")


      assert:
        json"""{
          "TYPE": "MonthRestriction",
          "months": []
        }""".as[MonthRestriction].toChecked == Left(Problem:
          "JSON DecodingFailure at : MonthRestriction must contain between 1 and 11 months")

      assert:
        json"""{
          "TYPE": "MonthRestriction",
          "months": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        }""".as[MonthRestriction].toChecked == Left(Problem:
          "JSON DecodingFailure at : MonthRestriction must contain between 1 and 11 months")

      testJson[SchemeRestriction](
        Unrestricted,
        json"""{
          "TYPE": "Unrestricted"
        }""")
  }

  "MonthRestriction" in:
    assert:
      SchemeRestriction.months(BitSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).orThrow.toString ==
        "Unrestricted"
    assert:
      SchemeRestriction.months(BitSet(1, 6, 12)).orThrow.toString ==
        "MonthRestriction(January June December)"
    assert:
      SchemeRestriction.months(BitSet(2, 3, 4)).orThrow.toString ==
        "MonthRestriction(February March April)"
    assert:
      SchemeRestriction.months(BitSet(5, 7, 8)).orThrow.toString ==
        "MonthRestriction(May July August)"
    assert:
      SchemeRestriction.months(BitSet(9, 10, 11)).orThrow.toString ==
        "MonthRestriction(September October November)"
