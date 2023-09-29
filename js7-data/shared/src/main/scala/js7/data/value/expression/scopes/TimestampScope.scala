package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}

final class TimestampScope(name: String, lazyTimestamp: => Option[Timestamp])
extends Scope:
  private lazy val maybeTimestamp = lazyTimestamp

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    functionCall match
      case FunctionCall(`name`, arguments) =>
        // now(format='yyyy-MM-dd', timezone='Antarctica/Troll'
        Some(arguments match {
          case Seq(
            Argument(formatExpr, None | Some("format")),
            Argument(timezoneExpr, None | Some("timezone"))) =>
            func(formatExpr, timezoneExpr)

          case Seq(Argument(formatExpr, None | Some("format"))) =>
            func2(formatExpr, None)

          case _ =>
            Left(InvalidFunctionArgumentsProblem(functionCall))
        })

      case _ =>
        super.evalFunctionCall(functionCall)

  private def func(formatExpr: Expression, timezoneExpr: Expression)(implicit scope: Scope) =
    for
      timezoneName <- timezoneExpr.evalAsString
      timezone = timezoneName.nonEmpty ? timezoneName
      result <- func2(formatExpr, timezone)
    yield result

  private def func2(formatExpr: Expression, timezone: Option[String])(implicit scope: Scope) =
    for
      format <- formatExpr.evalAsString
      result <- maybeTimestamp.fold(Checked(""))(_.format(format, timezone))
    yield StringValue(result)

  override def toString = s"TimestampScope('$name')"

object TimestampScope:
  def apply(name: String, lazyTimestamp: => Option[Timestamp]): Scope =
    new TimestampScope(name, lazyTimestamp)
