package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.proxy.javaapi.utils.VavrConversions._
import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}

@javaApi
trait JJsonable[A <: JJsonable[A]] extends JavaWrapper
{
  protected type Underlying

  protected def underlying: Underlying

  protected def companion: JJsonable.Companion[A]

  final def toJsonString: String = {
    val companion = this.companion
    val u = underlying.asInstanceOf[companion.Underlying]
    companion.jsonEncoder.apply(u).compactPrint
  }
}

@javaApi
object JJsonable
{
  trait Companion[A <: JJsonable[A]]
  {
    type Underlying = A#Underlying

    def apply(underlying: A#Underlying): A

    implicit def jsonEncoder: Encoder[A#Underlying]
    implicit def jsonDecoder: Decoder[A#Underlying]

    def fromJsonString(jsonString: String): VEither[Problem, A] =
      io.circe.parser.parse(jsonString).toChecked
        .flatMap(_.as[A#Underlying].toChecked map apply)
        .asVavr
  }
}
