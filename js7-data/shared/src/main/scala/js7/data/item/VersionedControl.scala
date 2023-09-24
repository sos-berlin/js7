package js7.data.item

trait VersionedControl extends UnsignedItem
{
  protected type Self <: VersionedControl

  val companion: VersionedControl.Companion[Self]
  final def key: companion.Key = id
  def id: UnsignedVersionedItemId[companion.Path]

  final def path: companion.Path = key.path
}

object VersionedControl
{
  type Companion_ = Companion[? <: VersionedControl]

  trait Companion[A <: VersionedControl] extends UnsignedItem.Companion[A]
  {
    type Item <: A

    type Path <: VersionedControlPath
    val Path: VersionedControlPath.Companion[Path]

    type ItemId = UnsignedVersionedItemId[Path]

    type Key = ItemId
    final def Key: UnsignedItemKey.Companion[Key] =
      Path.VersionedControlIdCompanion

    implicit def self: Companion[A] = this
  }
}
