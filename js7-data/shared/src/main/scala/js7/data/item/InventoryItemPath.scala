package js7.data.item

import io.circe.{Codec, DecodingFailure, HCursor, Json}
import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString
import js7.base.problem.Problem
import js7.base.standards.Js7PathValidating
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.{checkedCast, implicitClass}
import js7.data.item.InventoryItemPath._
import scala.reflect.ClassTag

trait InventoryItemPath extends GenericString
{
  def companion: Companion[_ <: InventoryItemPath]

  final def isAssignableToAgent =
    this.isInstanceOf[InventoryItemPath.AssignableToAgent]

  final def toTypedString: String =
    s"${companion.pathTypeName}:$string"

  override def toString = toTypedString
}

object InventoryItemPath
{
  abstract class Companion[P <: InventoryItemPath: ClassTag] extends Js7PathValidating[P]
  {
    def itemTypeName: String = name stripSuffix "Path"
    def pathTypeName: String = itemTypeName
    final val itemPathClass: Class[P] = implicitClass[P]
  }

  type AnyCompanion = Companion[_ <: InventoryItemPath]

  trait AssignableToAgent {
    this: InventoryItemPath =>
  }

  def jsonCodec[P <: InventoryItemPath: ClassTag](companions: Iterable[Companion[_ <: P]]): Codec[P] =
    new Codec[P] {
      private val typeToCompanion = companions.toKeyedMap(_.pathTypeName)

      def apply(a: P) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for {
          string <- c.as[String]
          prefixAndPath <- string indexOf ':' match {
            case i if i > 0 => Right((string.take(i), string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in InventoryItemPath: $string", c.history))
          }
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemPath <- typeToCompanion
            .get(prefix)
            .toRight(Problem.pure(s"Unrecognized type prefix in InventoryItemPath: $prefix"))
            .flatMap(_.checked(path))
            .flatMap(checkedCast[P](_))
            .toDecoderResult(c.history)
        } yield itemPath
    }
}
