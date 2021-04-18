package js7.data.orderwatch

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemId

final case class OrderWatchId(string: String) extends UnsignedSimpleItemId
{
  protected type Self = OrderWatchId

  val companion = OrderWatchId
}

object OrderWatchId extends UnsignedSimpleItemId.Companion[OrderWatchId]
{
  def itemTypeName = "OrderWatch"

  @javaApi
  def of(id: String) = apply(id)

  override protected def unchecked(string: String) =
    new OrderWatchId(string)
}
