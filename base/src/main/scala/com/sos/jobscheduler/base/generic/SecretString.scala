package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.sprayjson.SprayJson.implicits.RichJsValue
import spray.json.{JsString, JsValue, RootJsonFormat}

/**
  * @author Joacim Zschimmer
  */
final case class SecretString(string: String) {
  override def toString = "SecretString"
}

object SecretString {

  object implicits {
    // Import explicitly, it's secret.
    implicit object jsonFormat extends RootJsonFormat[SecretString] {
      def write(o: SecretString) = JsString(o.string)
      def read(jsValue: JsValue) = SecretString(jsValue.asString)
    }
  }

  implicit val StringAsSecretString: As[String, SecretString] =
    As(SecretString.apply)
}
