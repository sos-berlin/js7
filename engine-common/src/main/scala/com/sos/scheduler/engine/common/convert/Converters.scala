package com.sos.scheduler.engine.common.convert

import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
object Converters {

  trait To[V, W] extends (V ⇒ W)

  object To {
    def apply[V, W](toW: V ⇒ W) = new To[V, W] { def apply(v: V) = toW(v) }
  }

  implicit object StringTo extends To[String, String] {
    def apply(o: String) = o
  }

  implicit object StringToBoolean extends To[String, Boolean] {
    def apply(o: String) = o.toBoolean
  }

  implicit object StringToInt extends To[String, Int] {
    def apply(o: String) = Integer.parseInt(o)
  }

  implicit object StringToPath extends To[String, Path] {
    def apply(o: String) = Paths.get(o)
  }
}
