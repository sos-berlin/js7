package com.sos.jobscheduler.data.filebased

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
    def fileBased: FileBased

    final def path: FileBased#Path = id.path

    def id: FileBasedId[FileBased#Path] = fileBased.id

    def toShortString = s"$productPrefix($id)"
  }
  object AddedOrChanged {
    def unapply(o: AddedOrChanged) = Some(o.fileBased)
  }

  final case class Added(fileBased: FileBased) extends AddedOrChanged {
    require(!fileBased.id.path.isAnonymous, "Added event requires a path")
    require(!fileBased.id.versionId.isAnonymous, s"VersionId must not be anonymous in $toString")
  }

  final case class Updated(fileBased: FileBased) extends AddedOrChanged {
    require(!fileBased.id.path.isAnonymous, "FileChangedChanged event requires a path")
    require(!fileBased.id.versionId.isAnonymous, s"VersionId must not be anonymous in $toString")
  }

  final case class Deleted(path: TypedPath) extends Change {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }
}
