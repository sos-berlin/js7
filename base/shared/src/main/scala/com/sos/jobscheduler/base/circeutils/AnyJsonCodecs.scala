package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.utils.Collections.RichMap
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import io.circe.{Decoder, Encoder, Json, JsonObject}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object AnyJsonCodecs {

  object implicits {
    implicit val MapJsonEncoder: Encoder[Map[String, Any]] = anyToJson
    implicit val MapJsonDecoder: Decoder[Map[String, Any]] =
      c ⇒ Right(jsonToAny(c.value).asInstanceOf[Map[String, Any]])
  }

  def anyToJson(value: Any): Json =
    value match {
      case v: String ⇒ Json.fromString(v)
      case v: Boolean ⇒ Json.fromBoolean(v)
      case v: Integer ⇒ Json.fromInt(v)
      case v: Short ⇒ Json.fromInt(v.toInt)
      case v: Long ⇒ Json.fromLong(v)
      case v: Float ⇒ Json.fromFloatOrNull(v)
      case v: Double ⇒ Json.fromDoubleOrNull(v)
      case null ⇒ Json.Null
      case v: Map[_, _] ⇒ mapToJson(v.asInstanceOf[Map[String, Any]])
      case v: java.util.Map[_, _] ⇒ mapToJson(v.asInstanceOf[java.util.Map[String, Any]].asScala.toMap)
      case v: Array[_] ⇒ Json.fromValues(v map anyToJson)
      case v: Iterable[_] ⇒ Json.fromValues(v map anyToJson)
      case v: java.lang.Iterable[_] ⇒ Json.fromValues(v.asScala map anyToJson)
      case v: Json ⇒ v
      case v: java.math.BigDecimal ⇒ Json.fromBigDecimal(v)
      case v ⇒ sys.error(s"Unsupported type for JSON serialization: ${v.getClass.getName}")
    }

  def mapToJson(m: Map[String, Any]): Json =
    Json.fromJsonObject(JsonObject.fromMap(m map { case (k, v) ⇒ k → anyToJson(v) }))

  def jsonToAny(json: Json): Any =
    json.asBoolean match {
      case Some(o) ⇒ o: Boolean
      case None ⇒
        json.asNumber match {
          case Some(number) ⇒
            number.toInt match {
              case Some(int) ⇒ int: Int
              case None ⇒
                number.toLong match {
                  case Some(long) ⇒ long: Long
                  case None ⇒
                    //number.toBigInt match {
                    //  case Some(bigInt) ⇒ bigInt: BigInt
                    //  case None ⇒
                        number.toBigDecimal match {
                          case Some(decimal) ⇒ decimal: BigDecimal
                          case None ⇒ sys.error(s"JSON type ${number.getClass.simpleScalaName} cannot be converted to YAML")
                        }
                    //}
                }
            }
          case None ⇒
            json.asString match {
              case Some(o) ⇒ o: String
              case None if json.isNull ⇒ null
              case None ⇒
                json.asObject match {
                  case Some(o) ⇒ o.toMap filter (!_._2.isNull)/*remove None*/ mapValuesStrict jsonToAny
                  case None ⇒
                    json.asArray match {
                      case Some(o) ⇒ o map jsonToAny: Seq[Any]
                      case None ⇒ throw new RuntimeException(s"Unsupported Circe JSON type: ${json.getClass.simpleName}")
                    }
                }
            }
        }
    }
}
