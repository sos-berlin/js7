package js7.data.item

/**
  * @author Joacim Zschimmer
  */
sealed trait RepoChange
{
  def path: ItemPath
}

object RepoChange
{
  //final case class VersionAdded(versionId: VersionId) extends RepoChange

  sealed trait Change extends RepoChange {
    def path: ItemPath
  }

  sealed trait AddedOrChanged extends Change with Product {
    val item: VersionedItem

    final def path: item.Path = id.path

    def id: VersionedItemId[item.Path] = item.id

    def toShortString = s"$productPrefix($id)"
  }
  object AddedOrChanged {
    def unapply(o: AddedOrChanged) = Some(o.item)
  }

  final case class Added(item: VersionedItem) extends AddedOrChanged {
    require(!item.id.path.isAnonymous, "Added event requires a path")
    //require(!item.id.versionId.isAnonymous, s"VersionId is required in $toString")
  }

  final case class Changed(item: VersionedItem) extends AddedOrChanged {
    require(!item.id.path.isAnonymous, "FileChangedChanged event requires a path")
    //require(!item.id.versionId.isAnonymous, s"VersionId is required in $toString")
  }

  final case class Deleted(path: ItemPath) extends Change {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }
}
