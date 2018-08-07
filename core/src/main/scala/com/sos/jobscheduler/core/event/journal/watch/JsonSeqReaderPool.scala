package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.{Logger, ScalaConcurrentHashSet}
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, SeekableInputStream}
import com.sos.jobscheduler.core.event.journal.watch.JsonSeqReaderPool._
import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
private[watch] final class JsonSeqReaderPool(file: Path)
{
  private val freeReaders = new ConcurrentLinkedQueue[InputStreamJsonSeqReader]
  private val lentReaders = new ScalaConcurrentHashSet[InputStreamJsonSeqReader]
  @volatile
  private var closed = false
  @volatile
  private var _lastUsed = Timestamp.Epoch

  def close(): Unit = {
    closed = true
    val (availables, lent): (Set[InputStreamJsonSeqReader], Set[InputStreamJsonSeqReader]) =
      synchronized {
        (freeReaders.asScala.toSet, lentReaders.toSet)
      }
    if (lent.nonEmpty) {
      logger.debug(s"Closing '$toString' while ${lent.size}× opened")
    }
    (availables ++ lent) foreach (_.close())  // Force close readers, for Windows to unlock the file - required for testing
  }

  def borrowReader(): InputStreamJsonSeqReader = {
    if (closed) throw new ClosedException(file)

    var result: InputStreamJsonSeqReader = null
    synchronized {
      result = freeReaders.poll()
      if (result != null) {
        lentReaders += result
      }
    }
    if (result == null) {
      result = new InputStreamJsonSeqReader(SeekableInputStream.openFile(file)) {  // Exception when file has been deleted
        override def close() = {
          logger.trace(s"Close  $file")
          synchronized {
            freeReaders.remove(this)
            lentReaders -= this
          }
          super.close()
        }
        override def toString = s"InputStreamJsonSeqReader(${file.getFileName})"
      }
      logger.trace(s"Opened $file")
      // When close is called now the reader will not be closed. Good enough for JobScheduler use case.
      lentReaders += result
    }
    result
  }

  def returnReader(reader: InputStreamJsonSeqReader): Unit = {
    _lastUsed = Timestamp.now
    synchronized {
      if (!reader.isClosed) {
        freeReaders.add(reader)
      }
      lentReaders -= reader
    }
  }

  def size = synchronized { freeReaders.size + lentReaders.size }

  def isLent = lentReaders.nonEmpty

  def lastUsedAt = this._lastUsed
}

object JsonSeqReaderPool {
  private val logger = Logger(getClass)
}
