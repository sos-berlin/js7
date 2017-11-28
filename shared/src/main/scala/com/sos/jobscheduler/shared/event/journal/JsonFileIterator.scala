package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.shared.common.jsonseq.InputStreamJsonSeqIterator
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class JsonFileIterator(expectedHeader: JsonJournalHeader, convertInputStream: InputStream ⇒ InputStream, file: Path)
extends AutoCloseable with Iterator[Json] {

  private val in = new FileInputStream(file)
  private val bufferedIn = new BufferedInputStream(in)
  private val converted = convertInputStream(bufferedIn)
  private val iterator = new InputStreamJsonSeqIterator(if (converted eq bufferedIn) bufferedIn else new BufferedInputStream(converted))

  if (iterator.hasNext) {
    checkHeader(iterator.next().asJson)
  }

  def hasNext = iterator.hasNext

  def next() = iterator.next()

  def close() = {
    in.close()
    converted.close()
  }

  private def checkHeader(headerJson: Json): Unit = {
    val header = headerJson.as[JsonJournalHeader] match {
      case Right(o) ⇒ o
      case Left(t) ⇒
        throw new RuntimeException(s"Not a valid JobScheduler journal file: $file. Expected header ${headerJson.compactPrint}", t)
    }
    if (header.version != expectedHeader.version) {
      sys.error(s"Journal has version ${header.version} but ${expectedHeader.version} is expected. Incompatible journal file: $file")
    }
  }
}

object JsonFileIterator {
  object Empty extends AutoCloseable with Iterator[Json] {
    def hasNext = false
    def next() = throw new NoSuchElementException
    def close() = ()
  }
}
