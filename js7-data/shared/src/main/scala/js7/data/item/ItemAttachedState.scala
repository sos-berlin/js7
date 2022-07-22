package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass

sealed trait ItemAttachedState
{
  def isAttachableOrAttached: Boolean
}

object ItemAttachedState
{
  case object Attachable
  extends NotDetached {
    def isAttachableOrAttached = true
  }

  final case class Attached(itemRevision: Option[ItemRevision])
  extends NotDetached {
    def isAttachableOrAttached = true
  }
  object Attached {
    private val none = new Attached(None)

    implicit val jsonCodec: Codec.AsObject[Attached] = deriveCodec[Attached]

    def apply(itemRevision: Option[ItemRevision]): Attached =
      if (itemRevision.isEmpty)
        none
      else
        new Attached(itemRevision)
  }

  case object Detachable
  extends NotDetached {
    def isAttachableOrAttached = false
  }

  case object Detached
  extends ItemAttachedState {
    def isAttachableOrAttached = false
  }

  sealed trait NotDetached
  extends ItemAttachedState
  {
    def toShortString: String =
      getClass.simpleScalaName
  }
  object NotDetached {
    implicit val jsonCodec: TypedJsonCodec[NotDetached] = TypedJsonCodec(
      Subtype(Attachable),
      Subtype[Attached],
      Subtype(Detachable))
  }

  sealed trait AttachableOrAttached extends NotDetached
  object AttachableOrAttached {
    implicit val jsonCodec: TypedJsonCodec[NotDetached] = TypedJsonCodec(
      Subtype(Attachable),
      Subtype[Attached])
  }
}
