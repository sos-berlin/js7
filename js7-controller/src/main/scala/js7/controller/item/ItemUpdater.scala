package js7.controller.item

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.controller.VerifiedUpdateItems
import js7.data.crypt.SignedItemVerifier
import js7.data.item.SignableItem
import monix.eval.Task

trait ItemUpdater:
  def signedItemVerifier: SignedItemVerifier[SignableItem]

  def updateItems(verifiedUpdateItems: VerifiedUpdateItems): Task[Checked[Completed]]
