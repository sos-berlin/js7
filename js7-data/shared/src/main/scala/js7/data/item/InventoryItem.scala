package js7.data.item

import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.item.InventoryItem.Companion
import scala.collection.View
import scala.reflect.ClassTag

trait InventoryItem:
  protected type Self <: InventoryItem

  val companion: Companion[Self]

  def key: companion.Key

  def path: companion.Path

  def itemRevision: Option[ItemRevision]

  final def keyAndRevision: (InventoryItemKey, Option[ItemRevision]) =
    (key, itemRevision)

  def toInitialItemState: companion.ItemState

  /** Only if this Item is dedicated to an Agent.
    * An Item may be
    * <ul>
    *   <li>dedicated to an Agent (like FileWatch) or else
    *   <li>depend on Order's Agent (like Workflow, JobResource) or else
    *   <li>be used at Controller only (like Lock).
    * </ul>*/
  def dedicatedAgentPath: Option[AgentPath] = None

  def referencedItemPaths: View[InventoryItemPath] =
    View.empty

  // Accelerate usage in Set[InventoryItem], for example in AgentDriver's CommandQueue
  override def hashCode: Int = 31 * key.hashCode + itemRevision.hashCode


object InventoryItem:

  given Ordering[InventoryItem] = Ordering.by(_.key)

  type Companion_ = Companion[? <: InventoryItem]

  trait Companion[A <: InventoryItem]:
    type Item <: A
    def cls: Class[A]

    val typeName: String = getClass.simpleScalaName

    protected lazy val typeNameAliases: Seq[String] =
      Path.itemTypeNameAliases

    type Key <: InventoryItemKey
    def Key: InventoryItemKey.Companion[Key]

    type Path <: InventoryItemPath
    val Path: InventoryItemPath.Companion[Path]

    type ItemState <: InventoryItemState

    def subtype: Subtype[A] =
      Subtype[A](jsonCodec, aliases = typeNameAliases)(using ClassTag(cls))

    implicit def jsonCodec: Codec.AsObject[A]

    def jsonEncoder: Encoder.AsObject[A] =
      jsonCodec

    def jsonDecoder: Decoder[A] =
      jsonCodec

    override def toString: String = typeName

  def jsonCodec(companions: Seq[Companion_]): TypedJsonCodec[InventoryItem] =
    TypedJsonCodec(companions.map(_.subtype)*)
