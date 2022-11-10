package js7.base.log

import js7.base.log.CorrelIdLog4JThreadContextMap.*
import org.apache.logging.log4j.spi.{CopyOnWrite, ReadOnlyThreadContextMap, ThreadContextMap}
import org.apache.logging.log4j.util.StringMap

final class CorrelIdLog4JThreadContextMap
extends ThreadContextMap
with ReadOnlyThreadContextMap
// CopyOnWrite seems to mean that getReadOnlyContextData returns an immutable map.
// Then Log4j do not make a copy.*/
with CopyOnWrite
{
  def clear() = {}

  def put(key: String, value: String) =
    putSuppressedCount += 1

  def remove(key: String) = {}

  def isEmpty = false

  def get(key: String) =
    key match {
      case CorrelIdKey =>
        getCount += 1
        CorrelId.current.fixedWidthString

      case _ =>
        null
    }

  def containsKey(key: String) =
    key == CorrelIdKey

  def getImmutableMapOrNull =
    getCopy

  def getCopy: java.util.Map[String, String] = {
    getCopyCount += 1
    java.util.Collections.singletonMap(CorrelIdKey, CorrelId.local().fixedWidthString)
  }

  def getReadOnlyContextData: StringMap = {
    getReadOnlyContextDataCount += 1
    new CorrelIdLog4jStringMap(CorrelId.local())
  }
}

object CorrelIdLog4JThreadContextMap
{
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"

  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getCopyCount = 0L
  private var getReadOnlyContextDataCount = 0L

  private val isDebug: Boolean =
    sys.props.get("log4j2.debug") match {
      case Some("" | "true") => true
      case _ => false
    }

  def initialize(): Unit = {
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")
  }

  def statistics: String =
    s"$getReadOnlyContextDataCount× getReadOnlyContextData, " +
      s"${CorrelIdLog4jStringMap.forEachCount}× forEach, " +
      s"$getCopyCount× getCopy, " +
      s"$getCount× get, " +
      s"$putSuppressedCount× put (suppressed)"

  def logStatistics(): Unit =
    Logger[this.type].debug(statistics)

  private def debug(string: => String): Unit =
    if (isDebug) println(myClassName + " - " + string)

  private def myClassName =
    classOf[CorrelIdLog4JThreadContextMap].getName.stripSuffix("$")
}
