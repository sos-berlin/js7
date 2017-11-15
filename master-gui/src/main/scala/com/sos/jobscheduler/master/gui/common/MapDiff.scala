package com.sos.jobscheduler.master.gui.common

/**
  * @author Joacim Zschimmer
  */
final case class MapDiff[K, V] private(addedOrUpdated: Map[K, V], removed: Set[K]) {
  require((removed & addedOrUpdated.keySet).isEmpty, "MapDiff: addedOrUpdated and removed are not disjunct")

  def applyTo(variables: Map[K, V]): Map[K, V] =
    variables -- removed ++ addedOrUpdated
}

object MapDiff {
  private val Empty = new MapDiff(Map(), Set())

  def empty[K, V]: MapDiff[K, V] =
    Empty.asInstanceOf[MapDiff[K, V]]

  def apply[K, V](addedOrUpdated: Map[K, V] = Map[K, V]()) =
    new MapDiff[K, V](addedOrUpdated, removed = Set[K]())

  def apply[K, V](addedOrUpdated: Map[K, V], removed: Set[K]): MapDiff[K, V] =
    if (addedOrUpdated.isEmpty && removed.isEmpty)
      empty
    else
      new MapDiff(addedOrUpdated, removed)

  def addedOrUpdated[K, V](added: Map[K, V]): MapDiff[K, V] =
    MapDiff(added, Set())

  def diff[K, V](from: Map[K, V], to: Map[K, V]): MapDiff[K, V] =
    MapDiff(
      addedOrUpdated = to filter { case (k, v) ⇒ from.get(k) forall v.!= },
      removed = from.keySet -- to.keySet)
}
