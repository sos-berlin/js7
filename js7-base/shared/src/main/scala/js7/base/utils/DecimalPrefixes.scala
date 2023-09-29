package js7.base.utils

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import scala.math.{multiplyExact, toIntExact}

/**
  * @author Joacim Zschimmer
  */
object DecimalPrefixes
{
  private val PrefixToFactor = Map(
    'k' -> 1000,
    'M' -> (1000*1000),
    'G' -> (1000*1000*1000))

  def toInt(string: String): Checked[Int] =
    toLong(string)
      .flatMap(long =>
        catchExpected[ArithmeticException](
          toIntExact(long)))

  def toLong(string: String): Checked[Long] =
    if string.nonEmpty && string.last.isLetter then {
      val prefix = string.last
      PrefixToFactor.get(prefix)
        .toChecked(Problem(
          s"Unknown SI prefix: '$prefix', expected one of ${PrefixToFactor.keys.mkString(", ")}"))
        .flatMap(factor =>
          catchExpected[Exception](
            multiplyExact(
              string.take(string.length - 1).toLong,
              factor)))
    } else
      catchExpected[NumberFormatException](string.toLong)
}
