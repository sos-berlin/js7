package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import fastparse.all._

/**
  * @author Joacim Zschimmer
  */
object Parsers {
  object ops {
    implicit final class RichParser[A](private val underlying: Parser[A]) extends AnyVal {
      /** Parses the whole string, return a `Checked`. */
      def checkedParse(string: String): Checked[A] =
        (underlying ~ End).parse(string) match {
          case Parsed.Success(expr, _) ⇒ Valid(expr)
          case o: Parsed.Failure ⇒ Invalid(Problem.fromEager(o.msg))
          //case o: Parsed.Failure ⇒ Invalid(Problem.fromEager(s"Error at position 0+${o.index}: ${o.lastParser} - ${o.extra}"))
        }
    }
  }
}
