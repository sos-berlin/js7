package com.sos.jobscheduler.master.gui.common

import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.vdomNodeFromString
import scala.language.implicitConversions
import simulacrum.typeclass

@typeclass trait ToTagMod[T] {
  def toTagMod(f: T): TagMod
}

object ToTagMod {
  def toTagMod[A](f: A ⇒ TagMod): ToTagMod[A] =
    a ⇒ f(a)

  /** A [[ToTagMod]] using object toString */
  def fromToString[A]: ToTagMod[A] =
    a ⇒ vdomNodeFromString(a.toString)
}
