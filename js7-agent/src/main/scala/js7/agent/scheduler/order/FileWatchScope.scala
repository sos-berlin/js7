package js7.agent.scheduler.order

import cats.syntax.semigroup._
import java.util.regex.Matcher
import js7.agent.scheduler.order.FileWatchScope._
import js7.data.orderwatch.OrderWatchPath
import js7.data.value.StringValue
import js7.data.value.expression.scopes.NowScope
import js7.data.value.expression.{Scope, ValueSearch}
import scala.util.control.NonFatal

private final class FileWatchScope private(orderWatchPath: OrderWatchPath, matchedMatcher: Matcher)
extends Scope
{
  import ValueSearch.{LastOccurred, Name}

  override def findValue(search: ValueSearch) =
    Right(search match {
      // $orderWatchPath
      case ValueSearch(LastOccurred, Name("orderWatchPath")) =>
        Some(StringValue(orderWatchPath.string))

      // $0, $1, ...
      case ValueSearch(LastOccurred, Name(NumberRegex(nr))) =>
        try {
          val index = nr.toInt
          if (index >= 0 && index <= matchedMatcher.groupCount)
            Option(StringValue(matchedMatcher.group(index)))
          else
            None
        } catch { case NonFatal(_) =>
          None
        }

      case _ => None
    })
}

object FileWatchScope
{
  def apply(orderWatchPath: OrderWatchPath, matchedMatcher: Matcher): Scope = {
    val a: Scope = new FileWatchScope(orderWatchPath, matchedMatcher)
    a |+| NowScope()
  }

  private val NumberRegex = "([0-9]+)".r
}
