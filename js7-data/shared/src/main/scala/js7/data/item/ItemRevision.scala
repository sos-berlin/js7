package js7.data.item

import js7.base.generic.GenericLong

/** ItemRevision distinguishes the versions of an SimpleItem.
  * <p>
  * Unlike VersionedItem, a SimpleItem does not have a VersionId.
  * To allow and detect changes, the Controller provides this ItemRevision.
  **/
final case class ItemRevision(number: Long) extends GenericLong:
  def next = copy(number + 1)


object ItemRevision extends GenericLong.Companion[ItemRevision]:
  val Initial = ItemRevision(0)

  protected def unchecked(number: Long) =
    new ItemRevision(number)
