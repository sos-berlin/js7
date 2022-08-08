package js7.common.guice

import com.google.inject.{Injector, Key}
import izumi.reflect.Tag
import js7.base.utils.ScalaUtils.{implicitClass, implicitTypeRepr}
import js7.common.guice.Guices.typeLiteral
import scala.reflect.ClassTag

object GuiceImplicits {

  implicit final class RichInjector(private val delegate: Injector) extends AnyVal
  {
    def instance[A: Tag]: A =
      try delegate.getInstance(Key.get(typeLiteral[A]))
      catch { case e: ClassNotFoundException =>
        throw new ClassNotFoundException(s"instance[${implicitTypeRepr[A]}]: $e", e)
      }

    def option[A: ClassTag]: Option[A] =
      Option(delegate.getExistingBinding(Key.get(implicitClass[A]))).map(_.getProvider.get)
  }
}
