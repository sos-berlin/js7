package com.sos.scheduler.engine.taskserver.task.filecollector

import com.google.common.collect.{AbstractIterator ⇒ GuavaAbstractIterator}
import java.io.BufferedReader
import scala.collection.JavaConversions.asScalaIterator

/**
 * @author Joacim Zschimmer
 */
final class LineCollector(reader: BufferedReader) {

  /**
   * @return the next batch of all available lines
   */
  def nextLinesIterator(): Iterator[String] = asScalaIterator(
    new GuavaAbstractIterator[String] {
      def computeNext() = reader.readLine() match {
        case null ⇒ endOfData()
        case o ⇒ o
      }
    })
}
