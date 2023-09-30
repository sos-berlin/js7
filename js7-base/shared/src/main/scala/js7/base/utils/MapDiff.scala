package js7.base.utils

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import js7.base.utils.MapDiff.*

final case class MapDiff[K, V] private(
  added: Map[K, V],
  updated: Map[K, V],
  deleted: Set[K]):

  if (added.keySet & updated.keySet).nonEmpty ||
      (added.keySet & deleted).nonEmpty ||
      (updated.keySet & deleted).nonEmpty then
    throw new IllegalArgumentException("MapDiff: duplicate keys")

  def applyTo(other: Map[K, V]): Map[K, V] =
    if isEmpty then
      other
    else
      (other.view.filterKeys(deleted) ++ updated ++ added).toMap

  def isEmpty =
    this == Empty

object MapDiff:
  private val Empty = new MapDiff(Map.empty, Map.empty, Set.empty)

  def empty[K, V]: MapDiff[K, V] =
    Empty.asInstanceOf[MapDiff[K, V]]

  def added[K, V](added: Map[K, V]) =
    apply(added, Map.empty, Set.empty)

  def apply[K, V](added: Map[K, V], updated: Map[K, V], deleted: Set[K]): MapDiff[K, V] =
    if added.isEmpty && updated.isEmpty && deleted.isEmpty then
      empty
    else
      new MapDiff(added, updated, deleted)

  def diff[K, V](from: collection.Map[K, V], to: collection.Map[K, V]): MapDiff[K, V] =
    MapDiff(
      added = to.view.filter { case (k, v) => !from.contains(k) }.toMap,
      updated = to.view.filter { case (k, v) => from.get(k) exists v.!= }.toMap,
      deleted = from.keySet.toSet diff to.keySet)

  implicit val StringDiffJsonEncoder: Encoder[MapDiff[String, String]] = deriveEncoder[MapDiff[String, String]]
  implicit val StringDiffJsonDecoder: Decoder[MapDiff[String, String]] = deriveDecoder[MapDiff[String, String]]
