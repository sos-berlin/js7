package js7.controller.repo

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.crypt.VersionedItemVerifier
import js7.data.item.VersionedItem
import monix.eval.Task

trait RepoUpdater
{
  def itemVerifier: VersionedItemVerifier[VersionedItem]

  def updateRepo(verifiedUpdateRepo: VerifiedUpdateRepo): Task[Checked[Completed]]
}
