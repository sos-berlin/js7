package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.Path
import javax.xml.transform.stream.StreamSource
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class FileSource(path: Path)
extends StreamSource
with AutoCloseable {

  private val in = new FileInputStream(path)
  setInputStream(new BufferedInputStream(in))

  def close(): Unit = in.close()
}
