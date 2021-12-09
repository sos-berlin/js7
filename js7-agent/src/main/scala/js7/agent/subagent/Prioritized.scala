package js7.agent.subagent

import js7.base.problem.Checked
import js7.base.utils.Collections.RichMap

final class Prioritized[K, V](
  private val kToV: Map[K, V],
  private val orderedKeys: IndexedSeq[K])
  (private val vToK: V => K,
    private val toPriority: V => Int)
{
  def insert(v: V): Checked[Prioritized[K, V]] =
    for (updated <- kToV.insert(vToK(v) -> v))
      yield copy(
        kToV = updated,
        orderedKeys =
          Prioritized.prioritySort(
            orderedKeys.view :+ vToK(v))(
            toPriority = k => toPriority(updated(k))))

  def remove(k: K): Prioritized[K, V] =
    copy(
      kToV = kToV.removed(k),
      orderedKeys = orderedKeys.filter(_ != k))

  def clear: Prioritized[K, V] =
    copy(Map.empty, Vector.empty)

  def selectNext(fixedPriority: FixedPriority, filter: V => Boolean): Option[V] = {
    val orderedValues = orderedKeys.view.map(kToV).filter(filter).toVector
    if (orderedValues.isEmpty)
      None
    else {
      val highestPrio = toPriority(orderedValues.head)
      val highest = orderedValues.takeWhile(v => toPriority(v) == highestPrio)
      val next = fixedPriority.next(n = highest.size,
        isEquivalent = (i, j) => toPriority(orderedValues(i)) == toPriority(orderedValues(j)))
      highest.drop(next).headOption.orElse(highest.headOption)
      //(highest.view.drop(next) ++ highest.view.take(next)).headOption
    }
  }

  private def copy(kToV: Map[K, V], orderedKeys: IndexedSeq[K]) =
    new Prioritized[K, V](kToV, orderedKeys)(vToK, toPriority)

  override def equals(other: Any) =
    other match {
      case o: Prioritized[K, V] @unchecked =>
        vToK.eq(o.vToK) && toPriority.eq(o.toPriority) &&
        kToV == o.kToV && orderedKeys == o.orderedKeys
      case _ => false
    }
}

private object Prioritized
{
  def empty[K, V](vToK: V => K, toPriority: V => Int) =
    new Prioritized[K, V](Map.empty, Vector.empty)(vToK, toPriority)

  def prioritySort[A](as: Iterable[A])(toPriority: A => Int): Vector[A] =
    as.view
      .map(a => a -> toPriority(a))
      .toVector
      .sortWith((a, b) => a._2 > b._2)
      .map(_._1)
}
