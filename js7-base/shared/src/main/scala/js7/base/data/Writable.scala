package js7.base.data

import java.io.OutputStream
import scala.language.implicitConversions
import simulacrum.typeclass

@typeclass
trait Writable[A]
{
  def writeToStream(a: A, out: OutputStream): Unit
}
