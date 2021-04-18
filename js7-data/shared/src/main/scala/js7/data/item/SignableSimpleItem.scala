package js7.data.item

trait SignableSimpleItem extends SimpleItem with SignableItem
{
  protected type Self <: SignableSimpleItem

  val id: companion.Id
  //val id: SignableSimpleItemId
  val companion: SignableSimpleItem.Companion[Self]
}

object SignableSimpleItem
{
  type Companion_ = Companion[_ <: SignableSimpleItem]

  trait Companion[A <: SignableSimpleItem] extends SimpleItem.Companion[A] with SignableItem.Companion[A] {
    type Id <: SignableSimpleItemId
    val Id: js7.data.item.SignableSimpleItemId.Companion[Id]
  }
}
