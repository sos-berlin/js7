package js7.base.utils

import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.VectorOperators.EQ
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichThrowable

/** Vector operations in Java are still experimental and in a incubator phase.
  *
  * The User may enable them with the Java command line option:
  * <pre>--add-modules jdk.incubator.vector</pre>
  *
  * [[vectorIndexOf]] is quite fast!
  * */
object JavaVectors:

  private val logger = Logger[JavaVectors.type]

  /** A function, because it will fail if the JVM doesn't support it. */
  private inline def byteVectorSpecies = ByteVector.SPECIES_MAX

  extension (array: Array[Byte])
    /** Uses fast vector operation if possible. */
    def vectorIndexOf(byte: Byte): Int =
      vectorIndexOf(byte, 0, array.length)

    /** Uses fast vector operation if possible. */
    def vectorIndexOf(byte: Byte, start: Int): Int =
      vectorIndexOf(byte, start, array.length)

    /** Uses fast vector operation if possible. */
    def vectorIndexOf(byte: Byte, start: Int, end: Int): Int =
      if hasJavaIncubatorVectors then
        vectorIndexOf_(byte, start, end)
      else
        array.scalarIndexOf(byte, start, end)

    /** Uses fast vector operation, may fail if JVM doesn't support it.. */
    private def vectorIndexOf_(byte: Byte, start: Int, end: Int): Int =
      var offset = start
      val end_ = end min array.length
      val vectorLength = byteVectorSpecies.length
      while end_ - offset >= vectorLength do
        val index = ByteVector.fromArray(byteVectorSpecies, array, offset)
          .compare(EQ, byte)
          .firstTrue()
        if index < vectorLength then
          return offset + index
        offset += vectorLength
      array.scalarIndexOf(byte, offset, end)

    def scalarIndexOf(byte: Byte): Int =
      scalarIndexOf(byte, 0, array.length)

    def scalarIndexOf(byte: Byte, offset: Int): Int =
      scalarIndexOf(byte, offset, array.length)

    def scalarIndexOf(byte: Byte, start: Int, end: Int): Int =
      val end_ = end min array.length
      var i = start
      while i < end_ do
        if array(i) == byte then return i
        i = i + 1
      -1

  lazy val hasJavaIncubatorVectors: Boolean =
    val isSupportedJavaVersion = Set(25, 26)
    try
      isSupportedJavaVersion(Runtime.version.feature) &&
        ModuleLayer.boot.findModule("jdk.incubator.vector").isPresent &&
        locally:
          // Run a test to be sure
          val array = Array.fill[Byte](128*1024)('-')
          assert(array.vectorIndexOf_('-', 0, array.length) == 0)
          assert(array.vectorIndexOf_('x', 0, array.length) == -1)
          logger.info:
            s"Using fast Java incubator vectors with ${byteVectorSpecies.length} bytes per vector ☀️"
          true
    catch case t: Throwable =>
      logger.debug(t.toStringWithCauses)
      false
