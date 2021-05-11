package js7.base.io

import java.io.InputStream

final class EmptyInputStream extends InputStream
{
  def read() = -1

  override def available = 0
}
