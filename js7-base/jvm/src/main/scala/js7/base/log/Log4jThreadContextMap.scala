package js7.base.log

import js7.base.BuildInfo
import js7.base.log.Log4jThreadContextMap.*
import js7.base.system.startup.StartUp
import js7.base.utils.Atomic
import js7.base.utils.Tests.isTest
import org.apache.logging.log4j.spi.{CopyOnWrite, ReadOnlyThreadContextMap, ThreadContextMap}
import org.apache.logging.log4j.util.StringMap
import scala.collection.mutable

final class Log4jThreadContextMap
extends ThreadContextMap, ReadOnlyThreadContextMap, CopyOnWrite:
  // CopyOnWrite seems to mean that getReadOnlyContextData returns an immutable map.
  // Otherwise Log4j makes a copy with Log4jStringMap#forEach.

  private var lastLog4jStringMap: Log4jStringMap | Null = null
  private val lastKeyToValueVersion = Atomic(keyToValueVersion - 1)

  def clear(): Unit = ()

  def put(key: String, value: String): Unit =
    putSuppressedCount += 1

  def remove(key: String): Unit =
    assert(!isTest)

  def isEmpty = false

  // Not used
  def get(key: String): String =
    assert(!isTest)
    if key == CorrelIdKey then
      getCount += 1
      CorrelId.current.fixedWidthString
    else
      getOtherKey(key)

  // Not used
  def containsKey(key: String): Boolean =
    assert(!isTest)
    key == CorrelIdKey || keyToValue.contains(key)

  // Not used
  def getImmutableMapOrNull: java.util.Map[String, String] =
    assert(!isTest)
    getCopy

  // Not used
  def getCopy: java.util.Map[String, String] =
    assert(!isTest)
    getCopyCount += 1
    java.util.Collections.singletonMap(CorrelIdKey, CorrelId.local().fixedWidthString)

  def getReadOnlyContextData: StringMap =
    getReadOnlyContextDataCount += 1
    val last = lastLog4jStringMap
    val correlId = CorrelId.local()
    if last != null
      && last.correlId.eq(correlId)
      && keyToValueVersion == lastKeyToValueVersion.getAndSet(keyToValueVersion)
    then
      last
    else
      getReadOnlyContextDataCount2 += 1
      val r = new Log4jStringMap(correlId)
      lastLog4jStringMap = r
      r


object Log4jThreadContextMap:
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"

  private var keyToValueVersion = 0
  private val keyToValue = mutable.Map[String, String](
    "js7.version" -> BuildInfo.longVersion,
    "js7.longVersion" -> BuildInfo.longVersion,
    "js7.prettyVersion" -> BuildInfo.prettyVersion,
    "js7.system" -> StartUp.startUpLine())

  private val dummyNullCorrelId = CorrelId("__NULL__")

  // Counters are not accurate because not synchronized !!!
  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getCopyCount = 0L
  private var getReadOnlyContextDataCount = 0L
  private var getReadOnlyContextDataCount2 = 0L

  private[log] def getOtherKey(key: String): String =
    keyToValue.getOrElse(key, null)

  private val isDebug: Boolean =
    sys.props.get("log4j2.debug") match
      case Some("" | "true") => true
      case _ => false

  def initialize(name: String): Unit =
    keyToValue("js7.name") = name
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")

  private[log] def set(key: String, value: String) =
    keyToValue(key) = value
    keyToValueVersion += 1

  def statistics: String =
    val percent =
      val n = getReadOnlyContextDataCount
      if n == 0 then
        ""
      else
        val a = 100 * getReadOnlyContextDataCount2 / n
        s" ($a%)"
    s"$getReadOnlyContextDataCount×$percent getReadOnlyContextData, " +
      s"${Log4jStringMap.forEachCount}× forEach, " +
      s"$getCopyCount× getCopy, " +
      s"$getCount× get, " +
      s"$putSuppressedCount× put (suppressed)"

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

  private def debug(string: => String): Unit =
    if isDebug then println(myClassName + " - " + string)

  private def myClassName =
    classOf[Log4jThreadContextMap].getName.stripSuffix("$")
