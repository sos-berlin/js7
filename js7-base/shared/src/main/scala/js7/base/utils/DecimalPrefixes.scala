package js7.base.utils

import js7.base.problem.Checked._
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
      .flatMap(long => Checked.catchNonFatal(toIntExact(long)))

  def toLong(string: String): Checked[Long] =
    if (string.nonEmpty && string.last.isLetter) {
      val prefix = string.last
      PrefixToFactor.get(prefix).toChecked(Problem(s"Unknown SI prefix: '$prefix', expected one of ${PrefixToFactor.keys.mkString(", ")}"))
        .flatMap(factor => Checked.catchNonFatal(multiplyExact(string.take(string.length - 1).toLong, factor)))
    } else
      Checked.catchNonFatal(string.toLong)
}
