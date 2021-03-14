package js7.data.item

import js7.base.generic.GenericLong

final case class ItemRevision(number: Long) extends GenericLong
{
  def next = copy(number + 1)
}

object ItemRevision extends GenericLong.Companion[ItemRevision]
{
  val Initial = ItemRevision(0)

  protected def unchecked(number: Long) =
    new ItemRevision(number)
}
