package js7.base.io

import java.io.InputStream

final class EmptyInputStream extends InputStream:

  def read(): Int = -1

  override def available = 0
