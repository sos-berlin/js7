package js7.data.item

trait SignableSimpleItem extends SimpleItem, SignableItem:
  
  protected type Self <: SignableSimpleItem

  //val key: companion.Key
  //val id: SignableSimpleItemPath
  val companion: SignableSimpleItem.Companion[Self]


object SignableSimpleItem:
  type Companion_ = Companion[? <: SignableSimpleItem]

  trait Companion[A <: SignableSimpleItem]
  extends SimpleItem.Companion[A], SignableItem.Companion[A]:

    type Key <: SignableSimpleItemPath
    def Key: js7.data.item.SignableSimpleItemPath.Companion[Key]
