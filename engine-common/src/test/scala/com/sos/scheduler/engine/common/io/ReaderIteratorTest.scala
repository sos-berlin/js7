package com.sos.scheduler.engine.common.io

import java.io.StringReader
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ReaderIteratorTest extends FreeSpec {
  "ReaderIterator" in {
    newIterator().toList shouldEqual List('a', 'b', 'c')
  }

  "next and hasNext" in {
    val i = newIterator()
    i.hasNext shouldBe true
    i.hasNext shouldBe true
    i.next() shouldBe 'a'
    i.hasNext shouldBe true
    i.next() shouldBe 'b'
    i.hasNext shouldBe true
    i.next() shouldBe 'c'
    i.hasNext shouldBe false
    i.hasNext shouldBe false
    intercept[NoSuchElementException] { i.next() }
  }

  "empty" in {
    val i = new ReaderIterator(new StringReader(""))
    i.hasNext shouldBe false
    intercept[NoSuchElementException] { i.next() }
    intercept[NoSuchElementException] { new ReaderIterator(new StringReader("")).next() }
  }

  private def newIterator(): Iterator[Char] = new ReaderIterator(new StringReader("abc"))
}
