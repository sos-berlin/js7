package js7.base.utils

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/**
  * @author Joacim Zschimmer
  */
final case class MapDiff[K, V](changed: Map[K, V], deleted: Set[K] = Set.empty) {
  require((deleted & changed.keySet).isEmpty, "MapDiff: changed and deleted are not disjunct")

  def applyTo(variables: Map[K, V]): Map[K, V] =
    variables -- deleted ++ changed
}

object MapDiff {
  private val Empty = new MapDiff(Map(), Set())

  def empty[K, V]: MapDiff[K, V] =
    Empty.asInstanceOf[MapDiff[K, V]]

  def apply[K, V](changed: Map[K, V] = Map.empty[K, V]) =
    new MapDiff[K, V](changed, deleted = Set.empty[K])

  def apply[K, V](changed: Map[K, V], deleted: Set[K]): MapDiff[K, V] =
    if (changed.isEmpty && deleted.isEmpty)
      empty
    else
      new MapDiff(changed, deleted)

  def diff[K, V](from: Map[K, V], to: Map[K, V]): MapDiff[K, V] =
    MapDiff(
      changed = to filter { case (k, v) => from.get(k) forall v.!= },
      deleted = from.keySet -- to.keySet)

  implicit val StringDiffJsonEncoder: Encoder[MapDiff[String, String]] = deriveEncoder[MapDiff[String, String]]
  implicit val StringDiffJsonDecoder: Decoder[MapDiff[String, String]] = deriveDecoder[MapDiff[String, String]]
}
