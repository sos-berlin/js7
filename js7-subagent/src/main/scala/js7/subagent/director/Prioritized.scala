package js7.subagent.director

import js7.base.problem.{Checked, Problem}
import js7.subagent.director.Prioritized.prioritySort

private final class Prioritized[A] private(
  private val orderedKeys: IndexedSeq[A],
  private val toPriority: A => Int)
{
  private val fixedPriority = new FixedPriority

  override def equals(other: Any) =
    other match {
      case o: Prioritized[A] @unchecked =>
        toPriority.eq(o.toPriority) &&
          orderedKeys == o.orderedKeys
      case _ => false
    }

  def add(a: A): Checked[Prioritized[A]] =
    if (orderedKeys.contains(a))
      Left(Problem.pure(s"Duplicate value for Prioritized: $a"))
    else
      Right(copy(prioritySort(
        orderedKeys.view :+ a)(
        toPriority = k => toPriority(k))))

  def remove(a: A): Prioritized[A] =
    copy(orderedKeys.filter(_ != a))

  def clear: Prioritized[A] =
    copy(Vector.empty)

  def selectNext(filter: A => Boolean): Option[A] = {
    val orderedValues = orderedKeys.view.filter(filter).toVector
    if (orderedValues.isEmpty)
      None
    else {
      val highestPrio = toPriority(orderedValues.head)
      val highest = orderedValues.takeWhile(v => toPriority(v) == highestPrio)
      val next = fixedPriority.next(
        n = highest.size,
        isEquivalent = (i, j) => toPriority(orderedValues(i)) == toPriority(orderedValues(j)))
      highest.drop(next).headOption orElse highest.headOption
    }
  }

  private def copy(orderedKeys: IndexedSeq[A]) =
    if (orderedKeys == this.orderedKeys) {
      // Keep fixedPriority.index
      this
    } else
      new Prioritized[A](orderedKeys, toPriority)

  override def toString = s"Prioritized(${orderedKeys.mkString(" ")})"
}

private object Prioritized
{
  def empty[A](toPriority: A => Int) =
    new Prioritized[A](Vector.empty, toPriority)

  def apply[A](as: Iterable[A], toPriority: A => Int) =
    new Prioritized[A](
      prioritySort(as)(toPriority),
      toPriority)

  private[subagent] def prioritySort[A](as: Iterable[A])(toPriority: A => Int): Vector[A] =
    as.view
      .map(a => a -> toPriority(a))
      .toVector
      .sortWith((a, b) => a._2 > b._2)
      .map(_._1)
}
