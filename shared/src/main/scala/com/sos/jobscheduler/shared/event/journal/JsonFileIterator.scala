package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.shared.common.jsonseq.InputStreamJsonSeqIterator
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.Path
import scala.util.control.NonFatal
import spray.json.{JsObject, JsValue}

/**
  * @author Joacim Zschimmer
  */
final class JsonFileIterator(expectedHeader: JsonJournalHeader, convertInputStream: InputStream ⇒ InputStream, file: Path)
extends AutoCloseable with Iterator[JsValue] {

  private val in = new FileInputStream(file)
  private val bufferedIn = new BufferedInputStream(in)
  private val converted = convertInputStream(bufferedIn)
  private val iterator = new InputStreamJsonSeqIterator(if (converted eq bufferedIn) bufferedIn else new BufferedInputStream(converted))

  if (iterator.hasNext) {
    checkHeader(iterator.next().asJsObject)
  }

  def hasNext = iterator.hasNext

  def next() = iterator.next()

  def close() = {
    in.close()
    converted.close()
  }

  private def checkHeader(headerJson: JsObject): Unit = {
    val header = try headerJson.convertTo[JsonJournalHeader]
      catch { case NonFatal(t) ⇒
        throw new RuntimeException(s"Not a valid JobScheduler journal file: $file. Expected header ${headerJson.compactPrint}", t)
      }
    if (header.version != expectedHeader.version) {
      sys.error(s"Version '${header.version}' of journal file '$file' is not the expected '${expectedHeader.version}'")
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
