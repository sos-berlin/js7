package com.sos.scheduler.engine.base.generic

import com.sos.scheduler.engine.base.convert.As
import spray.json._

/**
  * @author Joacim Zschimmer
  */
trait GenericInt {
  def number: Int
}

object GenericInt {
  trait Companion[A <: GenericInt] {
    def apply(number: Int): A

    implicit val ordering: Ordering[A] = Ordering by { _.number }

    implicit def stringAsGenericInt: As[String, A] =
      As(o ⇒ apply(o.toInt))

    implicit object genericIntJsonFormat extends JsonFormat[A] {
      def write(o: A) = JsNumber(o.number)   // JavaScript uses only 53 bits due to conversion to Double !!!
      def read(json: JsValue) = apply(json.asInstanceOf[JsNumber].value.toIntExact)
    }
  }
}
