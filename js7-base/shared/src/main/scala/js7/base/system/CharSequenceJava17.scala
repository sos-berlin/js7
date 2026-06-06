package js7.base.system

/** Can be used in old Java versions to get a Java 25-like CharSequence with getChars method. */
trait CharSequenceJava17 extends CharSequence:
  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit
