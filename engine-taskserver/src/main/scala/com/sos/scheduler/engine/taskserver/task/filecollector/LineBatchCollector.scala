package com.sos.scheduler.engine.taskserver.task.filecollector

import com.google.common.collect.{AbstractIterator ⇒ GuavaAbstractIterator}
import java.io.BufferedReader
import org.scalactic.Requirements._
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.{immutable, mutable}

/**
 * @author Joacim Zschimmer
 */
final class LineBatchCollector(reader: BufferedReader, threshold: Int, lineWeight: String ⇒ Int = _.length + 1) {

  require(threshold > 0)

  /**
   * @return the next batch of all available lines
   */
  def nextBatchIterator(): Iterator[immutable.Seq[String]] = asScalaIterator(
    new GuavaAbstractIterator[immutable.Seq[String]] {
      def computeNext() = {
        val batch = mutable.Buffer[String]()
        @tailrec def continue(size: Int): Unit = {
          reader.readLine() match {
            case null ⇒
            case line ⇒
              batch += line
              val s = size + lineWeight(line)
              if (s < threshold) continue(s)
          }
        }
        continue(0)
        if (batch.nonEmpty) batch.toVector else endOfData()
      }
    })
}
