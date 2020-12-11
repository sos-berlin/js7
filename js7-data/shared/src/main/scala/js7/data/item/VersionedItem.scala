package js7.data.item

import js7.base.utils.ScalaUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
trait VersionedItem {
  type Self <: VersionedItem
  type Path = companion.Path
  type Id = VersionedItemId[Path]

  val companion: VersionedItem.Companion[Self]
  def id: VersionedItemId[Path]
  def withId(id: VersionedItemId[Path]): Self

  final def path: Path = id.path

  final def isAnonymous = id.isAnonymous

  final def withoutVersion: Self = withVersion(VersionId.Anonymous)

  final def withoutId: Self = withId(id = companion.itemPathCompanion.NoId)

  final def withVersion(v: VersionId): Self = withId(id = id.copy(versionId = v))

  def cast[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): A = {
    if (A != companion) throw new ClassCastException(s"Expected ${companion.itemPathCompanion.name}, but is: $path")
    this.asInstanceOf[A]
  }
}

object VersionedItem {
  type Companion_ = Companion[_ <: VersionedItem]

  trait Companion[A <: VersionedItem]
  {
    type ThisItem <: A
    type Path <: ItemPath

    val name = getClass.simpleScalaName

    def typeName = name

    def itemPathCompanion: ItemPath.Companion[Path]

    implicit def self: Companion[A] = this

    override def toString = name
  }
}
