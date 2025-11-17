package js7.data.orderwatch

import java.util.regex.Matcher
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.value.expression.Scope
import js7.data.value.{StringValue, Value}
import scala.util.Try

private[orderwatch] final class FileWatchScope(
  orderWatchPath: OrderWatchPath,
  matchedMatcher: Matcher)
extends Scope:

  override def namedValue(name: String): Option[Checked[Value]] =
    name match
      case "orderWatchPath" =>
        Some(Right(StringValue(orderWatchPath.string)))

      case name =>
        name.nonEmpty && name.forall(c => c >= '0' && c <= '9') thenMaybe
          Try(name.toInt)
            .toOption
            .flatMap: i =>
              if i < 0 || i > matchedMatcher.groupCount then
                None
              else
                Option(matchedMatcher.group(i)).map: group =>
                  Right(StringValue(group))

  override def toString = s"FileWatchScope($orderWatchPath, ${matchedMatcher.pattern})"


object FileWatchScope:
  def apply(orderWatchPath: OrderWatchPath, matchedMatcher: Matcher): Scope =
    new FileWatchScope(orderWatchPath, matchedMatcher)
