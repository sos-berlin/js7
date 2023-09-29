package js7.data.item

final case class PathRev[P <: InventoryItemPath](
  path: P,
  itemRevision: Option[ItemRevision]):
  override def toString =
    path.toString + itemRevision.fold("")(o => "~" + o.number)
