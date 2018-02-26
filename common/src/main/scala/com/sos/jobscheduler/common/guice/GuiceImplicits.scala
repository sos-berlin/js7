package com.sos.jobscheduler.common.guice

import com.google.inject.{Injector, Key}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

object GuiceImplicits {
  implicit final class RichInjector(private val delegate: Injector) extends AnyVal {

    def instance[A: ClassTag]: A =
      delegate.getInstance(implicitClass[A])

    def option[A: ClassTag]: Option[A] =
      Option(delegate.getExistingBinding(Key.get(implicitClass[A]))) map { _.getProvider.get }
  }
}
