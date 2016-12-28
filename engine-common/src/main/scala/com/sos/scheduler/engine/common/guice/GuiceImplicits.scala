package com.sos.scheduler.engine.common.guice

import com.google.inject.Injector
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

object GuiceImplicits {
  implicit class RichInjector(val delegate: Injector) extends AnyVal {

    def instance[A: ClassTag]: A =
      delegate.getInstance(implicitClass[A])
  }
}
