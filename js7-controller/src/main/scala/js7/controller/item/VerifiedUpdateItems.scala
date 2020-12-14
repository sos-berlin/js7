package js7.controller.item

import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.{ItemPath, SimpleItem, SimpleItemId, VersionId, VersionedItem}

final case class VerifiedUpdateItems(
  simple: VerifiedUpdateItems.Simple,
  maybeVersioned: Option[VerifiedUpdateItems.Versioned])

object VerifiedUpdateItems
{
  final case class Simple(
    items: Seq[SimpleItem],
    delete: Seq[SimpleItemId])

  final case class Versioned(
    versionId: VersionId,
    verifiedItems: Seq[Verified[VersionedItem]],
    delete: Seq[ItemPath])
}
