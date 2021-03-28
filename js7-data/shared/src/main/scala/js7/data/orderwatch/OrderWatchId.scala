package js7.data.orderwatch

import js7.base.annotation.javaApi
import js7.data.item.SimpleItemId

final case class OrderWatchId(string: String) extends SimpleItemId
{
  protected type Self = OrderWatchId

  val companion = OrderWatchId

  override def toString = s"OrderWatch:$string"
}

object OrderWatchId extends SimpleItemId.Companion[OrderWatchId]
{
  def itemTypeName = "OrderWatch"

  @javaApi
  def of(id: String) = apply(id)

  override protected def unchecked(string: String) =
    new OrderWatchId(string)
}
