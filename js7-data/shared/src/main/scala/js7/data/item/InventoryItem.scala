package js7.data.item

import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.item.InventoryItem.Companion
import scala.collection.View
import scala.reflect.ClassTag

trait InventoryItem
{
  protected type Self <: InventoryItem

  val companion: Companion[Self]

  def key: InventoryItemKey

  def path: InventoryItemPath

  def itemRevision: Option[ItemRevision]

  final def keyAndRevision: (InventoryItemKey, Option[ItemRevision]) =
    (key, itemRevision)

  /** Only if this Item is dedicated to an Agent.
    * An Item may be
    * <ul>
    *   <li>dedicated to an Agent (like FileWatch) or else
    *   <li>depend on Order's Agent (like Workflow, JobResource) or else
    *   <li>be used at Controller only (like AgentRef, Lock).
    * </ul>*/
  def dedicatedAgentPath: Option[AgentPath] = None

  def referencedItemPaths: View[InventoryItemPath] =
    View.empty

  // Accelerate usage in Set[InventoryItem], for example in AgentDriver's CommandQueue
  override def hashCode = 31 * key.hashCode + itemRevision.hashCode
}

object InventoryItem
{
  type Companion_ = Companion[_ <: InventoryItem]

  trait Companion[A <: InventoryItem]
  {
    type Item <: A
    def cls: Class[A]
    val typeName = getClass.simpleScalaName

    type Key <: InventoryItemKey
    val Key: InventoryItemKey.Companion[Key]

    type Path <: InventoryItemPath
    val Path: InventoryItemPath.Companion[Path]

    implicit def jsonCodec: Codec.AsObject[A]

    def subtype: Subtype[A] =
      Subtype(jsonCodec)(ClassTag(cls))

    def jsonEncoder: Encoder.AsObject[A] =
      jsonCodec

    def jsonDecoder: Decoder[A] =
      jsonCodec

    override def toString = typeName
  }

  def jsonCodec(companions: Seq[Companion_]): TypedJsonCodec[InventoryItem] =
    TypedJsonCodec(companions.map(_.subtype): _*)
}
