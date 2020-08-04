package js7.data.item

import js7.base.utils.ScalaUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
trait InventoryItem {
  type Self <: InventoryItem
  type Path = companion.Path
  type Id = ItemId[Path]

  val companion: InventoryItem.Companion[Self]
  def id: ItemId[Path]
  def withId(id: ItemId[Path]): Self

  final def path: Path = id.path

  final def isAnonymous = id.isAnonymous

  final def withoutVersion: Self = withVersion(VersionId.Anonymous)

  final def withoutId: Self = withId(id = companion.typedPathCompanion.NoId)

  final def withVersion(v: VersionId): Self = withId(id = id.copy(versionId = v))

  def cast[A <: InventoryItem](implicit A: InventoryItem.Companion[A]): A = {
    if (A != companion) throw new ClassCastException(s"Expected ${companion.typedPathCompanion.name}, but is: $path")
    this.asInstanceOf[A]
  }
}

object InventoryItem {
  type Companion_ = Companion[_ <: InventoryItem]

  trait Companion[A <: InventoryItem]
  {
    type ThisItem <: A
    type Path <: TypedPath

    val name = getClass.simpleScalaName

    def typeName = name

    def typedPathCompanion: TypedPath.Companion[Path]

    implicit def self: Companion[A] = this

    override def toString = name
  }
}
