package js7.common.scalautil.xmls

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.Path
import javax.xml.transform.stream.StreamSource

/**
  * @author Joacim Zschimmer
  */
final class FileSource(path: Path)
extends StreamSource, AutoCloseable:

  private val in = new FileInputStream(path.toFile)
  setInputStream(new BufferedInputStream(in))

  def close(): Unit = in.close()
