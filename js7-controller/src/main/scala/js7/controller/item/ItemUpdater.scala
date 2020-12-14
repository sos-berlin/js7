package js7.controller.item

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.crypt.VersionedItemVerifier
import js7.data.item.VersionedItem
import monix.eval.Task

trait ItemUpdater
{
  def versionedItemVerifier: VersionedItemVerifier[VersionedItem]

  def updateItems(verifiedUpdateItems: VerifiedUpdateItems): Task[Checked[Completed]]
}
