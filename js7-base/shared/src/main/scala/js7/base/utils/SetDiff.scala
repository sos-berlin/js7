package js7.base.utils

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import js7.base.utils.SetDiff.*

final case class SetDiff[A] private(added: Set[A], deleted: Set[A]):

  if (added & deleted).nonEmpty then
    throw new IllegalArgumentException("SetDiff: duplicate keys")

  def applyTo(other: Set[A]): Set[A] =
    if isEmpty then
      other
    else
      other -- deleted ++ added

  def isEmpty: Boolean =
    this == Empty


object SetDiff:
  private val Empty = new SetDiff(Set.empty, Set.empty)

  def empty[A]: SetDiff[A] =
    Empty.asInstanceOf[SetDiff[A]]

  def added[A](added: Set[A]): SetDiff[A] =
    apply(added, Set.empty)

  def apply[A](added: Set[A], deleted: Set[A]): SetDiff[A] =
    if added.isEmpty && deleted.isEmpty then
      empty
    else
      new SetDiff(added, deleted)

  def diff[A](from: collection.Set[A], to: collection.Set[A]): SetDiff[A] =
    SetDiff(
      added = to.view.filterNot(from.contains).toSet,
      deleted = from.toSet.diff(to))

  given Encoder[SetDiff[String]] = deriveEncoder
  given Decoder[SetDiff[String]] = deriveDecoder
