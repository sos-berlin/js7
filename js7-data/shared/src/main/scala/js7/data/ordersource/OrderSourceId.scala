package js7.data.ordersource

import js7.data.item.SimpleItemId

final case class OrderSourceId(string: String) extends SimpleItemId
{
  protected type Self = OrderSourceId

  val companion = OrderSourceId

  override def toString = s"OrderSource:$string"
}

object OrderSourceId extends SimpleItemId.Companion[OrderSourceId]
{
  def itemName = "OrderSource"

  override protected def unchecked(string: String) =
    new OrderSourceId(string)
}
