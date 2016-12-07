package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{MultipleSubtype, Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.data.event.KeyedEvent.{KeyFieldName, NoKey}
import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import scala.reflect.ClassTag
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class KeyedTypedEventJsonFormat[E <: Event: ClassTag](keyedSubtypes: Vector[KeyedSubtype[_ <: E]])
extends RootJsonFormat[KeyedEvent[E]] {

  private val typedJsonFormat: TypedJsonFormat[E] = TypedJsonFormat[E]()(keyedSubtypes map { _.toSubtype }: _*).asJsObjectJsonFormat
  private val eventClassToJsonFormat: Map[Class[_ <: E], JsonFormat[_]] = (keyedSubtypes map { o ⇒ o.eventClass → o.keyJsonFormat }).toMap

  private def classToKeyJsonFormat(c: Class[_ <: E]): JsonFormat[E#Key] = {
    eventClassToJsonFormat find { _._1 isAssignableFrom c } match {
      case Some((_, jsonFormat)) ⇒ jsonFormat.asInstanceOf[JsonFormat[E#Key]]
      case _ ⇒ sys.error(s"Unkown KeyedEvent[${c.getClass.getName}]")
    }
  }

  def canSerialize(o: KeyedEvent[E]) = typedJsonFormat canSerialize o.event

  def write(keyedEvent: KeyedEvent[E]) = {
    val jsValue = typedJsonFormat.write(keyedEvent.event)
    if (NoKey == keyedEvent.key)
      jsValue
    else {
      val keyJsonFormat: JsonFormat[E#Key] = classToKeyJsonFormat(keyedEvent.event.getClass)
      val fields = jsValue.asJsObject.fields
      require(!fields.contains(KeyFieldName), s"Serialized ${keyedEvent.getClass} must not contain a field '$KeyFieldName'")
      JsObject(fields + (KeyFieldName → keyedEvent.key.toJson(keyJsonFormat)))
    }
  }

  def read(jsValue: JsValue) = {
    val event = typedJsonFormat.read(jsValue)
    val keyJsonFormat = classToKeyJsonFormat(event.getClass)
    if (keyJsonFormat eq NoKey.NoKeyJsonFormat)
      KeyedEvent[E](NoKey.asInstanceOf[E#Key], event)
    else
      KeyedEvent[E](jsValue.asJsObject.fields(KeyFieldName).convertTo[E#Key](keyJsonFormat), event)
  }

  def typeNameToClass: Map[String, Class[_ <: E]] = typedJsonFormat.typeNameToClass

  implicit val eventTypedJsonFormat: TypedJsonFormat[E] =
    TypedJsonFormat()(keyedSubtypes map { _.toSubtype }: _*)

  implicit def anyEventJsonFormat[EE <: E]: TypedJsonFormat[EE] =
    eventTypedJsonFormat.asInstanceOf[TypedJsonFormat[EE]]
}

object KeyedTypedEventJsonFormat {
  final class KeyedSubtype[E <: Event: ClassTag] private(
    private[KeyedTypedEventJsonFormat] val keyJsonFormat: JsonFormat[E#Key],
    eventJsonFormat: RootJsonFormat[E]) {

    private[KeyedTypedEventJsonFormat] def eventClass = implicitClass[E]

    private[KeyedTypedEventJsonFormat] def toSubtype =
      eventJsonFormat match {
        case o: TypedJsonFormat[E] ⇒ new MultipleSubtype[E](classes = o.classes, jsonFormat = o)
        case _ ⇒ Subtype[E](eventJsonFormat)
      }
  }

  object KeyedSubtype {
    def apply[E <: Event: RootJsonFormat: ClassTag](implicit keyedJsonFormat: JsonFormat[E#Key]) =
      new KeyedSubtype[E](keyedJsonFormat, implicitly[RootJsonFormat[E]])
  }
}
