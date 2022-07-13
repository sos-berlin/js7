package js7.data.item

trait VersionedControl extends UnsignedItem
{
  protected type Self <: VersionedControl
  type Path = companion.Path

  val companion: VersionedControl.Companion[Self]
  final def key = id
  def id: UnsignedVersionedItemId[Path]

  final def path: Path = key.path
}

object VersionedControl
{
  type Companion_ = Companion[_ <: VersionedControl]

  trait Companion[A <: VersionedControl] extends UnsignedItem.Companion[A]
  {
    type Item <: A

    type Path <: VersionedControlPath
    val Path: VersionedControlPath.Companion[Path]

    type ItemId = UnsignedVersionedItemId[Path]

    type Key = ItemId
    final lazy val Key: UnsignedItemKey.Companion[Key] =
      Path.VersionedControlIdCompanion

    implicit def self: Companion[A] = this
  }
}
