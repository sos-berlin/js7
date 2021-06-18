package js7.agent.scheduler.order

import java.util.regex.Matcher
import js7.agent.scheduler.order.FileWatchScope._
import js7.data.orderwatch.OrderWatchPath
import js7.data.value.StringValue
import js7.data.value.expression.{Scope, ValueSearch}
import scala.util.control.NonFatal

private final class FileWatchScope private(
  orderWatchPath: OrderWatchPath,
  matchedMatcher: Matcher)
extends Scope
{
  import ValueSearch.{LastOccurred, Name}

  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    search match {
      // $orderWatchPath
      case ValueSearch(LastOccurred, Name("orderWatchPath")) =>
        Right(Some(StringValue(orderWatchPath.string)))

      // $0, $1, ...
      case ValueSearch(LastOccurred, Name(NumberRegex(nr))) =>
        Right(
          try {
            val index = nr.toInt
            if (index >= 0 && index <= matchedMatcher.groupCount)
              Option(StringValue(matchedMatcher.group(index)))
            else
              None
          } catch { case NonFatal(_) =>
            None
          })

      case _ =>
        super.findValue(search)
    }

  override def toString = s"FileWatchScope($orderWatchPath, ${matchedMatcher.pattern})"
}

object FileWatchScope
{
  def apply(orderWatchPath: OrderWatchPath, matchedMatcher: Matcher): Scope = {
    new FileWatchScope(orderWatchPath, matchedMatcher)
  }

  private val NumberRegex = "([0-9]+)".r
}
