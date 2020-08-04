package js7.data.item

/**
  * @author Joacim Zschimmer
  */
sealed trait RepoChange
{
  def path: TypedPath
}

object RepoChange
{
  //final case class VersionAdded(versionId: VersionId) extends RepoChange

  sealed trait Change extends RepoChange {
    def path: TypedPath
  }

  sealed trait AddedOrChanged extends Change with Product {
    def item: InventoryItem

    final def path: InventoryItem#Path = id.path

    def id: ItemId[InventoryItem#Path] = item.id

    def toShortString = s"$productPrefix($id)"
  }
  object AddedOrChanged {
    def unapply(o: AddedOrChanged) = Some(o.item)
  }

  final case class Added(item: InventoryItem) extends AddedOrChanged {
    require(!item.id.path.isAnonymous, "Added event requires a path")
    //require(!item.id.versionId.isAnonymous, s"VersionId is required in $toString")
  }

  final case class Updated(item: InventoryItem) extends AddedOrChanged {
    require(!item.id.path.isAnonymous, "FileChangedChanged event requires a path")
    //require(!item.id.versionId.isAnonymous, s"VersionId is required in $toString")
  }

  final case class Deleted(path: TypedPath) extends Change {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }
}
