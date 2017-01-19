package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{CanSerialize, HasOwnTypeField, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import scala.reflect.ClassTag
import spray.json._

/**
  * A [[Event]] enriched with a `key` designating the respective object.
  *
  * @author Joacim Zschimmer
  */
final case class KeyedEvent[+E <: Event](key: E#Key, event: E)

object KeyedEvent {
  private[event] val KeyFieldName = "key"

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

  implicit def jsonFormat[E <: Event: TypedJsonFormat](implicit keyJsonFormat: JsonFormat[E#Key]) =
    new KeyedEventJsonFormat[E]

  final class KeyedEventJsonFormat[E <: Event: TypedJsonFormat](implicit keyJsonFormat: JsonFormat[E#Key])
  extends RootJsonFormat[KeyedEvent[E]] with CanSerialize[KeyedEvent[E]] with HasOwnTypeField[E] {

    private val eJsonFormat = implicitly[TypedJsonFormat[E]].asJsObjectJsonFormat

    def classToJsonWriter = Map[Class[_], RootJsonWriter[_]](classOf[KeyedEvent[_]] → this)

    def subtypeNames = eJsonFormat.subtypeNames

    def typeNameToJsonReader = eJsonFormat.typeNameToJsonReader

    def typeNameToClass = eJsonFormat.typeNameToClass

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
