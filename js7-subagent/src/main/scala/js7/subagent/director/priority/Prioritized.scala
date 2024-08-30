package js7.subagent.director.priority

import js7.subagent.director.priority.Prioritized.prioritySort

private[director] final class Prioritized[A] private(
  private val orderedKeys: Vector[A],
  private val toPriority: A => Int):

  private val mutableSamePriorityRoundRobin = new MutableSamePriorityRoundRobin

  override def equals(other: Any) =
    other match
      case o: Prioritized[A @unchecked] =>
        toPriority.eq(o.toPriority) &&
          orderedKeys == o.orderedKeys
      case _ => false

  def insertFirst(a: A): Prioritized[A] =
    replaceAll(a +: orderedKeys.filter(_ != a))

  def add(a: A): Prioritized[A] =
    replaceAll(orderedKeys.filter(_ != a) :+ a)

  private def replaceAll(keys: Seq[A]): Prioritized[A] =
    copy(prioritySort(keys)(toPriority = k => toPriority(k)))

  def remove(a: A): Prioritized[A] =
    copy(orderedKeys.filter(_ != a))

  def clear: Prioritized[A] =
    copy(Vector.empty)

  def selectNext(filter: A => Boolean): Option[A] =
    val orderedValues = orderedKeys.filter(filter)
    if orderedValues.isEmpty then
      None
    else
      val highestPrio = toPriority(orderedValues.head)
      val highest = orderedValues.takeWhile(v => toPriority(v) == highestPrio)
      val next = mutableSamePriorityRoundRobin.next(
        n = highest.size,
        samePriority = (i, j) => toPriority(orderedValues(i)) == toPriority(orderedValues(j)))
      highest.drop(next).headOption orElse highest.headOption

  private def copy(orderedKeys: Vector[A]) =
    if orderedKeys == this.orderedKeys then
      // Keep mutableSamePriorityRoundRobin.index
      this
    else
      new Prioritized[A](orderedKeys, toPriority)

  override def toString = s"Prioritized(${orderedKeys.mkString(" ")})"


private[director] object Prioritized:

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
