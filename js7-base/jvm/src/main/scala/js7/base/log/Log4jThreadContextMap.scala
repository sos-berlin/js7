package js7.base.log

import java.util.concurrent.ConcurrentHashMap
import js7.base.BuildInfo
import js7.base.log.Log4jThreadContextMap.*
import js7.base.system.startup.StartUp
import js7.base.utils.Lazy
import js7.base.utils.Tests.isTest
import org.apache.logging.log4j.spi.{CopyOnWrite, ReadOnlyThreadContextMap, ThreadContextMap}
import org.apache.logging.log4j.util.StringMap

final class Log4jThreadContextMap
extends ThreadContextMap, ReadOnlyThreadContextMap, CopyOnWrite:
  // CopyOnWrite seems to mean that getReadOnlyContextData returns an immutable map.
  // Otherwise, Log4j makes a copy with Log4jStringMap#forEach.

  private var lastLog4jStringMap: Log4jStringMap | Null = null
  private var lastKeyToValueVersion = keyToValueVersion - 1

  def clear(): Unit = ()

  def put(key: String, value: String): Unit =
    putSuppressedCount += 1

  def remove(key: String): Unit =
    assert(!isTest)

  def isEmpty = false

  // Not used
  def get(key: String): String | Null =
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
    val v = keyToValueVersion
    if last != null && last.correlId.eq(correlId) && v == lastKeyToValueVersion then
      last
    else
      lastKeyToValueVersion = v
      log4jStringMapCount += 1
      val r = Log4jStringMap(correlId)
      lastLog4jStringMap = r
      r


object Log4jThreadContextMap:
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"

  private val myClassName = classOf[Log4jThreadContextMap].getName.stripSuffix("$")

  private var keyToValueVersion = 0
  private val keyToValue = new ConcurrentHashMap[String, String | Lazy[String]]:
    put("js7.version", BuildInfo.longVersion)
    put("js7.longVersion", BuildInfo.longVersion)
    put("js7.prettyVersion", BuildInfo.prettyVersion)
    put("js7.system", Lazy(StartUp.startUpLine()))

  private val dummyNullCorrelId = CorrelId("__NULL__")

  // Counters are not accurate because not synchronized !!!
  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getCopyCount = 0L
  private var getReadOnlyContextDataCount = 0L
  private var log4jStringMapCount = 0L

  private[log] def getOtherKey(key: String): String | Null =
    keyToValue.get(key) match
      case o @ (null | _: String) => o
      case lzy: Lazy[String] => lzy.value

  private val isDebug: Boolean =
    sys.props.get("log4j2.debug") match
      case Some("" | "true") => true
      case _ => false

  def initialize(name: String): Unit =
    keyToValue.put("js7.serverId", name) // May be overwritten later by a more specific value
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")

  private[log] def set(key: String, value: String): Unit =
    keyToValue.put(key, value)
    keyToValueVersion += 1

  def statistics: String =
    val percent =
      val n = getReadOnlyContextDataCount
      if n == 0 then
        ""
      else
        val a = 100 * log4jStringMapCount / n
        s" ($a%)"
    s"$log4jStringMapCount$percent Log4jStringMap, " +
      s"${Log4jStringMap.forEachCount}× forEach, " +
      s"$getCopyCount× getCopy, " +
      s"$getCount× get, " +
      s"$putSuppressedCount× put (suppressed)"

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

  private def debug(string: => String): Unit =
    if isDebug then println(s"$myClassName - $string")
