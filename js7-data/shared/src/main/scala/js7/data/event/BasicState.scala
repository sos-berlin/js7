package js7.data.event

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.BasicState.*

trait BasicState[S <: BasicState[S]] {
  def companion: Companion[S]
}

object BasicState {
  trait Companion[S <: BasicState[S]] {
    implicit final val implicitBasicState: Companion[S] =
      this

    def name: String =
      getClass.shortClassName

    override def toString = name
  }
}
