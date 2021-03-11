package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.{AgentId, AttachedState}

trait SimpleItemEvent extends ItemEvent
{
  def id: SimpleItemId
}

object SimpleItemEvent
{
  sealed trait SimpleItemAddedOrChanged extends SimpleItemEvent {
    def item: SimpleItem
  }
  object SimpleItemAddedOrChanged {
    def unapply(event: SimpleItemAddedOrChanged) = Some(event.item)
  }

  final case class SimpleItemAdded(item: SimpleItem)
  extends SimpleItemAddedOrChanged
  {
    def id = item.id
  }

  final case class SimpleItemChanged(item: SimpleItem)
  extends SimpleItemAddedOrChanged
  {
    def id = item.id
  }

  final case class SimpleItemDeleted(id: SimpleItemId)
  extends SimpleItemEvent

  sealed trait SimpleItemAttachedStateChanged
  extends SimpleItemEvent {
    def agentId: AgentId
    def attachedState: Option[AttachedState]
  }
  object SimpleItemAttachedStateChanged {
    def unapply(event: SimpleItemAttachedStateChanged) =
      Some((event.id, event.agentId, event.attachedState))
  }

  final case class SimpleItemAttachable(id: SimpleItemId, agentId: AgentId)
  extends SimpleItemAttachedStateChanged {
    def attachedState = Some(AttachedState.Attaching)
  }

  final case class SimpleItemAttached(id: SimpleItemId, agentId: AgentId)
  extends SimpleItemAttachedStateChanged {
    def attachedState = Some(AttachedState.Attached)
  }

  //final case class SimpleItemDetached(id: SimpleItemId, agentId: AgentId)
  //extends SimpleItemAttachedStateChanged {
  //  def attachedState = None
  //}

  /** Agent only. */
  final case class SimpleItemAttachedToAgent(item: SimpleItem)
  extends SimpleItemEvent {
    def id = item.id
  }

  //final case class SimpleItemAttachmentFailed(id: SimpleItemId, agentId: AgentId, problem: Problem)
  //extends SimpleItemEvent

  def jsonCodec[A <: SimpleItem](companions: Seq[SimpleItem.Companion]): TypedJsonCodec[SimpleItemEvent] = {
    implicit val itemJsonCodec = SimpleItem.jsonCodec(companions)
    implicit val idJsonCodec = SimpleItemId.jsonCodec(companions.map(_.idCompanion))

    TypedJsonCodec(
      Subtype(deriveCodec[SimpleItemAdded]),
      Subtype(deriveCodec[SimpleItemChanged]),
      Subtype(deriveCodec[SimpleItemDeleted]),
      Subtype(deriveCodec[SimpleItemAttachable]),
      Subtype(deriveCodec[SimpleItemAttachedToAgent]),
      Subtype(deriveCodec[SimpleItemAttached]))
  }
}
