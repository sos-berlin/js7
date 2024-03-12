package js7.controller.item

import cats.effect.IO
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.controller.VerifiedUpdateItems
import js7.data.crypt.SignedItemVerifier
import js7.data.item.SignableItem

trait ItemUpdater:
  def signedItemVerifier: SignedItemVerifier[SignableItem]

  def updateItems(verifiedUpdateItems: VerifiedUpdateItems): IO[Checked[Completed]]
