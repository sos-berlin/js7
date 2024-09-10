package js7.data.subagent

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.typed.Subtype
import js7.data.item.{InventoryItemPath, ItemRevision, UnsignedItemPath, UnsignedSimpleItem}
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.NumericConstant
import scala.collection.View

final case class SubagentBundle(
  id: SubagentBundleId,
  subagentToPriority: Map[SubagentId, Expression],
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:
  protected type Self = SubagentBundle
  val companion: SubagentBundle.type = SubagentBundle

  def path: SubagentBundleId = id

  def rename(id: SubagentBundleId): SubagentBundle =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): SubagentBundle =
    copy(itemRevision = revision)

  def toInitialItemState: SubagentBundleState =
    SubagentBundleState(this)

  //override def dedicatedAgentPath = AgentPath cannot be determined here ???

  lazy val allPrioritiesArePure: Boolean =
    subagentToPriority.valuesIterator.forall(_.isPure)

  override def referencedItemPaths: View[InventoryItemPath] =
    subagentIds

  def subagentIds: View[SubagentId] =
    subagentToPriority.keySet.view



object SubagentBundle extends UnsignedSimpleItem.Companion[SubagentBundle]:

  type Key = SubagentBundleId
  def Key: SubagentBundleId.type = SubagentBundleId

  override type Path = Key
  val Path: UnsignedItemPath.Companion[SubagentBundleId] = Key

  type ItemState = SubagentBundleState

  val cls: Class[SubagentBundle] = classOf[SubagentBundle]

  override val jsonEncoder: Encoder.AsObject[SubagentBundle] = o =>
    JsonObject(
      "id" -> o.id.asJson,
      "subagentToPriority" -> o.subagentToPriority
        .view.mapValues:
          case NumericConstant(number) => number.asJson
          case expr => expr.asJson
        .toMap
        .asJson,
      "itemRevision" -> o.itemRevision.asJson)

  override val jsonDecoder: Decoder[SubagentBundle] =
    // Own Decoder[Expression] for compatibility with previous versions which
    // use a constant value instead of an expression.
    given Decoder[Expression] = c =>
      if c.value.isNumber then
        c.as[BigDecimal].map(NumericConstant(_))
      else
        Expression.jsonDecoder(c)

    c =>
      for
        id <- c.get[SubagentBundleId]("id")
        subagentToPriority <- c.get[Map[SubagentId, Expression]]("subagentToPriority")
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      yield
        SubagentBundle(id, subagentToPriority, itemRevision)

  implicit val jsonCodec: Codec.AsObject[SubagentBundle] =
    Codec.AsObject.from(jsonDecoder, jsonEncoder)

  override val subtype: Subtype[SubagentBundle] =
    Subtype[SubagentBundle](aliases = Seq("SubagentSelection"))
