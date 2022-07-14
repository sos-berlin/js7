package js7.data.parser

import fastparse.*
import js7.base.problem.{Checked, Problem}

/**
  * @author Joacim Zschimmer
  */
object Parsers
{
  /** Parses the whole string, return a `Checked`. */
  def checkedParse[T, V, R](string: String, parser: P[_] => P[T])
    (implicit whitespace: P[_] => P[Unit])
  : Checked[T] = {
    def complete(implicit ctx: P[_]): P[T] = parser(ctx) ~ End
    parse(string, complete(_)) match {
      case Parsed.Success(expr, _) => Right(expr)
      case o: Parsed.Failure => Left(Problem.pure(o.trace().msg))
      //case Parsed.Failure(lastParser, index, extra) => Left(Problem.pure(s"Error at position 0+$index: $lastParser - $extra"))
    }
  }
}
