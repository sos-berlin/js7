package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.base.generic.IsString
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.TagMod
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object RenderUtils {

  object implicits {
    implicit def isStringToTagMod[A <: IsString](a: A): TagMod =
      a.string
  }
}
