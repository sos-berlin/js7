package js7.controller.repo

import js7.data.crypt.InventoryItemVerifier.Verified
import js7.data.item.{InventoryItem, TypedPath, VersionId}
import scala.collection.immutable.Seq

final case class VerifiedUpdateRepo(
  versionId: VersionId,
  verifiedItem: Seq[Verified[InventoryItem]],
  delete: Seq[TypedPath])
