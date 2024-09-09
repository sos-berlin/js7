package js7.data.subagent

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import js7.data.item.{InventoryItemPath, ItemRevision, UnsignedItemPath, UnsignedSimpleItem}
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.NumericConstant
import scala.collection.View

final case class SubagentSelection(
  id: SubagentSelectionId,
  subagentToPriority: Map[SubagentId, Expression],
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:
  protected type Self = SubagentSelection
  val companion: SubagentSelection.type = SubagentSelection

  def path: SubagentSelectionId = id

  def rename(id: SubagentSelectionId): SubagentSelection =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): SubagentSelection =
    copy(itemRevision = revision)

  def toInitialItemState: SubagentSelectionState =
    SubagentSelectionState(this)

  //override def dedicatedAgentPath = AgentPath cannot be determined here ???

  lazy val allPrioritiesArePure: Boolean =
    subagentToPriority.valuesIterator.forall(_.isPure)

  override def referencedItemPaths: View[InventoryItemPath] =
    subagentIds

  def subagentIds: View[SubagentId] =
    subagentToPriority.keySet.view



object SubagentSelection extends UnsignedSimpleItem.Companion[SubagentSelection]:

  type Key = SubagentSelectionId
  def Key: SubagentSelectionId.type = SubagentSelectionId

  override type Path = Key
  val Path: UnsignedItemPath.Companion[SubagentSelectionId] = Key

  type ItemState = SubagentSelectionState

  val cls: Class[SubagentSelection] = classOf[SubagentSelection]

  override val jsonEncoder: Encoder.AsObject[SubagentSelection] = o =>
    JsonObject(
      "id" -> o.id.asJson,
      "subagentToPriority" -> o.subagentToPriority
        .view.mapValues:
          case NumericConstant(number) => number.asJson
          case expr => expr.asJson
        .toMap
        .asJson,
      "itemRevision" -> o.itemRevision.asJson)

  override val jsonDecoder: Decoder[SubagentSelection] =
    // Own Decoder[Expression] for compatibility with previous versions which
    // use a constant value instead of an expression.
    given Decoder[Expression] = c =>
      if c.value.isNumber then
        c.as[BigDecimal].map(NumericConstant(_))
      else
        Expression.jsonDecoder(c)

    c =>
      for
        id <- c.get[SubagentSelectionId]("id")
        subagentToPriority <- c.get[Map[SubagentId, Expression]]("subagentToPriority")
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      yield
        SubagentSelection(id, subagentToPriority, itemRevision)

  implicit val jsonCodec: Codec.AsObject[SubagentSelection] =
    Codec.AsObject.from(jsonDecoder, jsonEncoder)
