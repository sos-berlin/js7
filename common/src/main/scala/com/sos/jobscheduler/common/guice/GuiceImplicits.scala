package com.sos.jobscheduler.common.guice

import com.google.inject.Injector
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

object GuiceImplicits {
  implicit class RichInjector(val delegate: Injector) extends AnyVal {

    def instance[A: ClassTag]: A =
      delegate.getInstance(implicitClass[A])
  }
}
