package js7.data.job

import js7.base.generic.GenericString
import js7.data.agent.IncreasingPositiveLongs
import scala.math.abs
import scala.util.Random

/**
 * @see C++ Process_id
 * @author Joacim Zschimmer
 */
final case class TaskId private(string: String) extends GenericString:
  import js7.data.job.TaskId.*

  if !pattern.matcher(string).matches then
    throw new IllegalArgumentException(s"Invalid TaskId($string)")

  /**
   *  Index, only valid if string starts with number followed by a hypen, as generated by Agent.
   */
  def index: Long = string.takeWhile(_ != '-').toLong

  override def toString = s"TaskId($string)"


object TaskId extends GenericString.Checked_[TaskId]:
  private val pattern = "[A-Za-z0-9-][A-Za-z0-9._-]*".r.pattern  // Try to exclude any shell meta characters

  protected def unchecked(string: String) = new TaskId(string)

  def apply(index: Long, salt: Long) = new TaskId(s"$index-$salt")

  /**
   * Delivers [[TaskId]] with recognizable strictly increasing numbers.
   * The numbers itself are meaningless.
   */
  def newGenerator(start: Int = 1): Iterator[TaskId] =
    new IncreasingPositiveLongs(start = start, maximum = Int.MaxValue)
      .map(n => TaskId(index = n, salt = abs(Random.nextLong())))
