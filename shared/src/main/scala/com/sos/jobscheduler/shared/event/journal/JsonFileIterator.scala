package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.shared.common.jsonseq.InputStreamJsonSeqIterator
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.Path
import spray.json.{JsObject, JsValue}

/**
  * @author Joacim Zschimmer
  */
final class JsonFileIterator(expectedHeader: JsObject, convertInputStream: InputStream ⇒ InputStream, file: Path)
extends AutoCloseable with Iterator[JsValue] {

  private val in = new FileInputStream(file)
  private val iterator = new InputStreamJsonSeqIterator(new BufferedInputStream(convertInputStream(in)))

  if (iterator.hasNext) {
    checkHeader(iterator.next().asJsObject)
  }

  def hasNext = iterator.hasNext

  def next() = iterator.next()

  def close() = in.close()

  private def checkHeader(header: JsObject): Unit = {
    if (header != expectedHeader) {
      val readVersionOption = header.fields.get("version")
      val expectedVersion = expectedHeader.fields("version")
      if (!readVersionOption.contains(expectedVersion)) {
        sys.error(s"Version ${readVersionOption getOrElse "(missing)"} of file '$file' is not the expected '$expectedVersion'")
      } else {
        sys.error(s"Not a valid JobScheduler journal file: $file. Expected header ${expectedHeader.compactPrint}")
      }
    }
  }
}

object JsonFileIterator {
  object Empty extends AutoCloseable with Iterator[JsValue] {
    def hasNext = false
    def next() = throw new NoSuchElementException
    def close() = ()
  }
}
