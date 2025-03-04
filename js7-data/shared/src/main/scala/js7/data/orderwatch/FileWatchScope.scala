package js7.data.orderwatch

import java.util.regex.Matcher
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.value.expression.Scope
import js7.data.value.{StringValue, Value}
import scala.collection.MapView
import scala.util.Try

private[orderwatch] final class FileWatchScope(
  orderWatchPath: OrderWatchPath,
  matchedMatcher: Matcher)
extends Scope:

  override lazy val nameToCheckedValue: MapView[String, Right[Nothing, Value]] =
    new MapView:
      def get(key: String): Option[Right[Nothing, Value]] =
        key match
          case "orderWatchPath" =>
            Some(Right(StringValue(orderWatchPath.string)))

          case _ =>
            key.nonEmpty && key.forall(c => c >= '0' && c <= '9') thenMaybe
              Try(key.toInt)
                .toOption
                .flatMap(i =>
                  if i < 0 || i > matchedMatcher.groupCount then
                    None
                  else
                    Option(matchedMatcher.group(i))
                      .map(group => Right(StringValue(group))))

      def iterator: Iterator[(String, Right[Nothing, Value])] =
        get("orderWatchPath").iterator.map("orderWatchPath" -> _) ++
          (0 to matchedMatcher.groupCount).iterator
            .flatMap(i =>
              Option(matchedMatcher.group(i))
                .map(group => i.toString -> Right(StringValue(group))))

  override def toString = s"FileWatchScope($orderWatchPath, ${matchedMatcher.pattern})"


object FileWatchScope:
  def apply(orderWatchPath: OrderWatchPath, matchedMatcher: Matcher): Scope =
    new FileWatchScope(orderWatchPath, matchedMatcher)
