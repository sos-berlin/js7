package js7.data.item

/**
  * @author Joacim Zschimmer
  */
trait VersionedItem extends SignableItem:
  protected type Self <: VersionedItem
  type Path = companion.Path

  val companion: VersionedItem.Companion[Self]
  final def key = id
  def id: VersionedItemId[Path]
  def withId(id: VersionedItemId[Path]): Self

  final def itemRevision = None

  def path: Path = key.path

  final def isAnonymous = key.isAnonymous

  final def withoutVersion: Self = withVersion(VersionId.Anonymous)

  final def withoutId: Self = withId(id = companion.Path.NoId)

  final def withVersion(v: VersionId): Self =
    withId(id = key.copy(versionId = v))

  def cast[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): A =
    if A != companion then throw new ClassCastException(s"Expected ${companion.Path.name}, but is: $path")
    this.asInstanceOf[A]

object VersionedItem:
  type Companion_ = Companion[? <: VersionedItem]

  trait Companion[A <: VersionedItem] extends SignableItem.Companion[A]:
    type Item <: A

    type Path <: VersionedItemPath
    val Path: VersionedItemPath.Companion[Path]

    type ItemId = VersionedItemId[Path]
    type Key = ItemId
    final def Key: SignableItemKey.Companion[Key] = Path.VersionedItemIdCompanion

    implicit def self: Companion[A] = this
