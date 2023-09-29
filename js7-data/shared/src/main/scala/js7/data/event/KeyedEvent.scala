package js7.data.event

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.DecoderOK
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent.*
import scala.reflect.ClassTag

/**
  * An [[Event]] enriched with a `key` designating the respective object.
  */
final class KeyedEvent[+E <: Event](val event: E)(val key: event.keyCompanion.Key):

  override def hashCode =
    31 * key.hashCode + event.hashCode

  override def equals(o: Any) =
    o match
      case o: KeyedEvent[E] @unchecked =>
        key == o.key && event == o.event
      case _ => false

  override def toString = s"$keyPrefix$event"

  def toShortString = s"$keyPrefix${TypedJsonCodec.typeName(event.getClass)}"

  private def keyPrefix = (key != NoKey) ?? s"$key $Arrow "

type AnyKeyedEvent = KeyedEvent[Event]

val <-: = KeyedEvent

object KeyedEvent:
  private[event] val KeyFieldName = "Key"
  val Arrow = "<-:"

  type NoKey = NoKey.type
  case object NoKey:
    // Used as implicit argument, but not executed
    implicit val jsonCodec: Codec[NoKey] = Codec.from(
      _ => sys.error("NoKey.jsonCodec"),
      _ => sys.error("NoKey.jsonCodec"))

    override def toString = "NoKey"

    @javaApi
    def singleton(): NoKey.type = this

  @deprecated
  def apply[E <: Event](event: E)(key: event.keyCompanion.Key): KeyedEvent[E] =
    new KeyedEvent(event)(key)

  def apply[E <: NoKeyEvent](event: E): KeyedEvent[E] =
    new KeyedEvent[E](event)(NoKey)

  @javaApi
  def of[E <: NoKeyEvent](event: E) =
    new KeyedEvent[E](event)(NoKey)

  def any(key: Any, event: Event): AnyKeyedEvent =
    new KeyedEvent[Event](event)(key.asInstanceOf[event.keyCompanion.Key])

  def any(event: NoKeyEvent): AnyKeyedEvent =
    any(NoKey, event)

  def unapply[E <: Event](ke: KeyedEvent[E]): (ke.event.keyCompanion.Key, E) =
    ke.key -> ke.event

  implicit def jsonEncoder[E <: Event : Encoder.AsObject](using E: Event.KeyCompanion[? >: E])
    (using Encoder[E.Key])
  : Encoder.AsObject[KeyedEvent[E]] =
    keyedEvent => {
      val jsonObject = keyedEvent.event.asJsonObject
      keyedEvent.key match
        case NoKey => jsonObject
        case key =>
          //import E.keyJsonCodec
          require(!jsonObject.contains(KeyFieldName),
            s"Serialized ${keyedEvent.getClass} must not contain a field '$KeyFieldName'")
          (KeyFieldName -> key.asInstanceOf[E.Key].asJson) +: jsonObject
    }

  implicit def jsonDecoder[E <: Event: Decoder](using E: Event.KeyCompanion[? >: E])
    (using Decoder[E.Key])
  : Decoder[KeyedEvent[E]] =
    cursor => for
      key <- cursor.getOrElse[E.Key](KeyFieldName)(NoKey.asInstanceOf[E.Key])
      event <- cursor.as[E]
      _ <-
        if event.keyCompanion ne E then
          Left(DecodingFailure(s"${event.keyCompanion}, but $E expected", cursor.history))
        else
          DecoderOK
    yield
      any(key, event).asInstanceOf[KeyedEvent[E]]

  def typedJsonCodec[E <: Event: ClassTag](subtypes: KeyedEventTypedJsonCodec.KeyedSubtype[? <: E]*)
  : KeyedEventTypedJsonCodec[E] =
    KeyedEventTypedJsonCodec[E](subtypes*)
