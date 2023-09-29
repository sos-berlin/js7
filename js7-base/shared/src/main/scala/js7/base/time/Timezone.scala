package js7.base.time

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

final case class Timezone(string: String) extends GenericString

object Timezone extends GenericString.NonEmpty[Timezone]:
  val utc = new Timezone("UTC")

  protected def unchecked(string: String) = new Timezone(string)

  @javaApi
  def of(timezone: String): Timezone =
    Timezone(timezone)
