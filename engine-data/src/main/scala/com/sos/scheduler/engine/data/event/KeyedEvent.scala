package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{CanSerialize, HasOwnTypeField, MultipleSubtype, Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag
import spray.json._

final case class KeyedEvent[+E <: Event](key: E#Key, event: E)

object KeyedEvent {
  private val KeyFieldName = "key"

  object NoKey {
    implicit val NoKeyJsonFormat = new JsonFormat[NoKey.type] {
      def read(json: JsValue) = sys.error("NoKey")
      def write(obj: NoKey.type) = sys.error("NoKey")
    }

    override def toString = "NoKey"
  }

  def apply[E <: Event](event: E)(key: event.Key) = new KeyedEvent(key, event)

  def apply[E <: Event { type Key = NoKey.type }](event: E) = new KeyedEvent[E](NoKey, event)

  def of[E <: Event { type Key = NoKey.type }](event: E) = new KeyedEvent[E](NoKey, event)

  def typedJsonFormat[E <: Event: ClassTag](subtypes: KeyedSubtype[_ <: E]*) =
    new KeyedTypedEventJsonFormat[E](subtypes.toVector)

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

    implicit val eventTypedJsonFormat: TypedJsonFormat[E] = TypedJsonFormat()(keyedSubtypes map { _.toSubtype }: _*)
    implicit val anyEventJsonFormat = eventTypedJsonFormat.asInstanceOf[TypedJsonFormat[AnyEvent]]
  }

  final class KeyedSubtype[E <: Event: ClassTag] private(
    private[KeyedEvent] val keyJsonFormat: JsonFormat[E#Key],
    eventJsonFormat: RootJsonFormat[E]) {

    private[KeyedEvent] def eventClass = implicitClass[E]

    private[KeyedEvent] def toSubtype =
      eventJsonFormat match {
        case o: TypedJsonFormat[E @unchecked] ⇒ new MultipleSubtype[E](classes = o.classes, jsonFormat = o)
        case _ ⇒ Subtype[E](eventJsonFormat)
      }
  }

  object KeyedSubtype {
    def apply[E <: Event: RootJsonFormat: ClassTag](implicit keyedJsonFormat: JsonFormat[E#Key]) =
      new KeyedSubtype[E](keyedJsonFormat, implicitly[RootJsonFormat[E]])
  }

  def keyedEventJsonFormat[E <: Event: TypedJsonFormat](implicit keyJsonFormat: JsonFormat[E#Key]) =
    new KeyedEventJsonFormat[E]

  final class KeyedEventJsonFormat[E <: Event: TypedJsonFormat](implicit keyJsonFormat: JsonFormat[E#Key])
  extends RootJsonFormat[KeyedEvent[E]] with CanSerialize[KeyedEvent[E]] with HasOwnTypeField[E] {

    private val eJsonFormat = implicitly[TypedJsonFormat[E]].asJsObjectJsonFormat

    val classToJsonWriter = Map[Class[_], RootJsonWriter[_]](classOf[KeyedEvent[_]] → this)

    val typeToJsonReader = eJsonFormat.typeToJsonReader

    def canSerialize(o: KeyedEvent[E]) = eJsonFormat canSerialize o.event

    def write(keyedEvent: KeyedEvent[E]) = {
      val jsValue = keyedEvent.event.toJson(eJsonFormat)
      if (NoKey == keyedEvent.key)
        jsValue
      else {
        val fields = jsValue.asJsObject.fields
        require(!fields.contains(KeyFieldName), s"Serialized ${keyedEvent.getClass} must not contain a field '$KeyFieldName'")
        JsObject(fields + (KeyFieldName → keyedEvent.key.toJson))
      }
    }

    def read(json: JsValue) = {
      val jsObject = json.asJsObject
      val event = jsObject.convertTo[E](eJsonFormat)
      if (keyJsonFormat eq NoKey.NoKeyJsonFormat)
        KeyedEvent[E](NoKey.asInstanceOf[E#Key], event)
      else
        KeyedEvent[E](jsObject.fields(KeyFieldName).convertTo[E#Key], event)
    }
  }
}
