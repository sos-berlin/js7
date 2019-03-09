package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import fastparse._

/**
  * @author Joacim Zschimmer
  */
object Parsers
{
  /** Parses the whole string, return a `Checked`. */
  def checkedParse[T, V, R](string: String, parser: P[_] => P[T])
    (implicit s: Implicits.Sequencer[T, V, R], whitespace: P[_] => P[Unit])
  : Checked[T] = {
    def complete(implicit ctx: P[_]): P[T] = parser(ctx) ~ End
    parse(string, complete(_)) match {
      case Parsed.Success(expr, _) => Valid(expr)
      case o: Parsed.Failure => Invalid(Problem.pure(o.trace().msg))
      //case Parsed.Failure(lastParser, index, extra) => Invalid(Problem.pure(s"Error at position 0+$index: $lastParser - $extra"))
    }
  }
}
