package js7.common.guice

import com.google.inject.{Injector, Key}
import js7.base.utils.ScalaUtils.implicitClass
import js7.common.guice.Lings.typeLiteral
import scala.reflect.ClassTag

object GuiceImplicits {

  implicit final class RichInjector(private val delegate: Injector) extends AnyVal
  {
    def instance[A: Manifest]: A =
      delegate.getInstance(Key.get(typeLiteral[A]))

    def option[A: ClassTag]: Option[A] =
      Option(delegate.getExistingBinding(Key.get(implicitClass[A]))).map(_.getProvider.get)
  }
}
