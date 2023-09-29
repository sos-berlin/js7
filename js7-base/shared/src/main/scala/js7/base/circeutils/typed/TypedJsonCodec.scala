package js7.base.circeutils.typed

import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import js7.base.circeutils.typed.TypedJsonCodec.*
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonCodec[A](
  private val printName: String,
  private val subtypes: Seq[Subtype[A]])
extends Codec.AsObject[A]:
  val classToEncoder: Map[Class[?], Encoder.AsObject[A]] =
    subtypes
      .flatMap(_.classToEncoder)
      .uniqueToMap
      .withDefault(o =>
        throw new UnknownClassForJsonException(o.shortClassName, printName))

  val nameToDecoder: Map[String, Decoder[A]] =
    subtypes
      .flatMap(_.nameToDecoder)
      .uniqueToMap

  val nameToClass: Map[String, Class[A]] =
    subtypes
      .flatMap(_.nameToClass)
      .uniqueToMap

  private val _classToName: Map[Class[A], String] =
    nameToClass.map(o => o._2 -> o._1)

  private val classToNameJson: Map[Class[A], Json/*String*/] =
    _classToName.view.mapValues(Json.fromString).toMap

  /** Union. */
  def |[B](other: TypedJsonCodec[B]): TypedJsonCodec[Any] =
    val sameClasses = classToEncoder.keySet & other.classToEncoder.keySet
    if sameClasses.nonEmpty then throw new IllegalArgumentException(
      s"Union of TypedJsonCodec has non-unique classes: $sameClasses")

    val sameClassNames = nameToClass.keySet & other.nameToClass.keySet
    if sameClassNames.nonEmpty then throw new IllegalArgumentException(
      s"Union of TypedJsonCodec has non-unique decoder names: $sameClassNames")

    val sameDecoderNames = nameToDecoder.keySet & other.nameToDecoder.keySet
    if sameDecoderNames.nonEmpty then throw new IllegalArgumentException(
      s"Union of TypedJsonCodec has non-unique class names: $sameDecoderNames")

    new TypedJsonCodec[Any](
      s"$printName|${other.printName}",
      (subtypes ++ other.subtypes).asInstanceOf[Seq[Subtype[Any]]])

  def apply(c: HCursor) = decode(c)

  def encodeObject(a: A): JsonObject =
    classToEncoder(a.getClass).asInstanceOf[Encoder.AsObject[A]].encodeObject(a)

  def decode(c: HCursor): Decoder.Result[A] =
    c.get[String](TypeFieldName)
      .flatMap(name => nameToDecoder.get(name) match {
        case None => Left(unknownJsonTypeFailure(name, printName, c.history))
        case Some(decoder) => decoder.apply(c)
      })

  def canDeserialize(json: Json): Boolean =
    json.asObject match
      case Some(o) => o.toMap.get(TypeFieldName).flatMap(_.asString) exists nameToDecoder.contains
      case _ => false

  def typeName[A1 <: A](implicit A1: ClassTag[A1]): String =
    _classToName(A1.runtimeClass.asInstanceOf[Class[A]])

  def typeName[A1 <: A](a: A1): String =
    _classToName(a.getClass.asInstanceOf[Class[A]])

  def classToName(getClass: Class[? <: A]): String =
    _classToName(getClass.asInstanceOf[Class[A]])

  def classes[A2 <: A: ClassTag]: Set[Class[A2]] =
    classToEncoder.keySet collect:
      case c if implicitClass[A2] isAssignableFrom c => c.asInstanceOf[Class[A2]]

  def isOfType[A1 <: A: ClassTag](json: Json): Boolean =
    json.asObject
      .flatMap(_(TypeFieldName)) contains classToNameJson(implicitClass[A1].asInstanceOf[Class[A]])

  def jsonToClass(json: Json): Option[Class[A]] =
    json.asObject
      .flatMap(_(TypeFieldName)).flatMap(_.asString).flatMap(nameToClass.get)

  override def toString = printName

object TypedJsonCodec:
  val TypeFieldName = "TYPE"

  def typeField[A: ClassTag]: (String, Json) =
    TypeFieldName -> Json.fromString(typeName(implicitClass[A]))

  def typeName[A: ClassTag]: String =
    typeName(implicitClass[A])

  def typeName(cls: Class[?]): String =
    cls.simpleScalaName

  def apply[A: ClassTag](subtypes: Subtype[? <: A]*)
    (implicit enclosing: sourcecode.Enclosing)
  : TypedJsonCodec[A] =
    fromIterable(
      enclosing.value + ": TypedJsonCodec[" + implicitClass[A].shortClassName + "]",
      subtypes)

  def named[A: ClassTag](name: String, subtypes: Subtype[A]*)
  : TypedJsonCodec[A] =
    fromIterable(name, subtypes)

  def fromIterable[A: ClassTag](name: String, subtypes: Iterable[Subtype[? <: A]])
  : TypedJsonCodec[A] =
    new TypedJsonCodec[A](name, subtypes.toSeq.asInstanceOf[Seq[Subtype[A]]])

  implicit final class TypedJson(private val underlying: Json) extends AnyVal:
    def isOfType[A: TypedJsonCodec, A1 <: A: ClassTag]: Boolean =
      implicitly[TypedJsonCodec[A]].isOfType[A1](underlying)

    def toClass[A: TypedJsonCodec]: Option[Class[A]] =
      implicitly[TypedJsonCodec[A]].jsonToClass(underlying)

  final class UnknownClassForJsonException(subclassName: String, superclassName: String)
  extends NoSuchElementException("Class '" + subclassName + "' is not registered in " +
    (if superclassName.contains("TypedJsonCodec") then superclassName
    else s"TypedJsonCodec[$superclassName]}"))

  final def unknownJsonTypeFailure(typeName: String, superclassName: String, history: List[CursorOp]): DecodingFailure =
    DecodingFailure(s"""Unexpected JSON {"$TypeFieldName": "$typeName", ...} for $superclassName""", history)
