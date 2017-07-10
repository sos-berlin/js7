package com.sos.jobscheduler.base.utils

import spray.json.DefaultJsonProtocol._
import spray.json.{JsonFormat, RootJsonFormat}

/**
  * @author Joacim Zschimmer
  */
final case class MapDiff[K, V](addedOrUpdated: Map[K, V], removed: Set[K]) {
  require((removed & addedOrUpdated.keySet).isEmpty, "MapDiff: addedOrUpdated and removed are not disjunct")

  def applyTo(variables: Map[K, V]): Map[K, V] =
    variables -- removed ++ addedOrUpdated
}

object MapDiff {
  implicit val VariablesDiffJsonFormat: RootJsonFormat[MapDiff[String, String]] =
    jsonFormat[String, String]

  def apply[K, V](addedOrUpdated: Map[K, V] = Map[K, V]()) =
    new MapDiff[K, V](addedOrUpdated, removed = Set[K]())

  implicit def jsonFormat[K: JsonFormat, V: JsonFormat]: RootJsonFormat[MapDiff[K, V]] =
    jsonFormat2((addedOrUpdated: Map[K, V], removed: Set[K]) ⇒ new MapDiff(addedOrUpdated, removed))

  def addedOrUpdated[K, V](added: Map[K, V]): MapDiff[K, V] =
    MapDiff(added, Set())

  def diff[K, V](from: Map[K, V], to: Map[K, V]): MapDiff[K, V] =
    MapDiff(
      addedOrUpdated = to filter { case (k, v) ⇒ from.get(k) forall v.!= },
      removed = from.keySet -- to.keySet)
}
