package com.sos.scheduler.engine.common.guice

import com.google.inject.Injector
import scala.reflect.ClassTag

object GuiceImplicits {
  implicit class RichInjector(val delegate: Injector) extends AnyVal {

    def instance[A](implicit c: ClassTag[A]) = delegate.getInstance(c.runtimeClass).asInstanceOf[A]
  }
}
