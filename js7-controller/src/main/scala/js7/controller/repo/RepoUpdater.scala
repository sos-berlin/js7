package js7.controller.repo

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.crypt.InventoryItemVerifier
import js7.data.item.InventoryItem
import monix.eval.Task

trait RepoUpdater
{
  def itemVerifier: InventoryItemVerifier[InventoryItem]

  def updateRepo(verifiedUpdateRepo: VerifiedUpdateRepo): Task[Checked[Completed]]
}
