package js7.controller.repo

import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.{VersionedItem, ItemPath, VersionId}

final case class VerifiedUpdateRepo(
  versionId: VersionId,
  verifiedItem: Seq[Verified[VersionedItem]],
  delete: Seq[ItemPath])
