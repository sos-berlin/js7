package js7.base.time

import com.typesafe.config.{Config, ConfigException}
import js7.base.problem.Checked.catchExpected
import js7.base.problem.{Checked, Problem}
import monix.execution.Scheduler
import scala.jdk.CollectionConverters.*
import scala.jdk.DurationConverters.JavaDurationOps
import scala.util.Try

object DelayIterators:

  /**
   * The delay sequence may be provide as a non-empty list.
   * Then the last element is endlessly repeated.
   * Or a single duration can be given, which is endlessly repeated.
   */
  def fromConfig(config: Config, key: String)(implicit s: Scheduler): Checked[DelayIterator] =
    Checked
      .fromTry(Try(config.getDuration(key).toScala :: Nil))
      .orElse(catchExpected[ConfigException](
        config.getDurationList(key).asScala.view.map(_.toScala).toVector))
      .flatMap(durations =>
        if durations.isEmpty then
          Left(Problem(s"Setting '$key' must not be empty"))
        else
          Right(new DelayIterator(durations)))
