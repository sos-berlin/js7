package js7.data.item

import io.circe.{Decoder, Encoder}
import js7.data.item.SimpleItem._

trait SimpleItem
{
  val companion: Companion

  def id: companion.Id
}

object SimpleItem
{
  trait Companion
  {
    type Item <: SimpleItem
    type Id <: SimpleItemId

    implicit def jsonEncoder: Encoder.AsObject[Item]
    implicit def jsonDecoder: Decoder[Item]
  }
}
