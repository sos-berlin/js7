package js7.base.log.log4j

import java.util.concurrent.ConcurrentHashMap
import js7.base.BuildInfo
import js7.base.log.log4j.Log4jThreadContextMap.*
import js7.base.log.{CorrelId, Logger}
import js7.base.system.startup.StartUp
import js7.base.utils.ScalaUtils.flatten
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SystemPropertiesExtensions.asSwitch
import js7.base.utils.Tests.isTest
import js7.base.utils.{Lazy, ScalaUtils}
import org.apache.logging.log4j.spi.ThreadContextMap

final class Log4jThreadContextMap extends ThreadContextMap:

  private var lastImmutableMap: Log4jMap | Null = null
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

  def getImmutableMapOrNull: java.util.Map[String, String] =
    getImmutableMapOrNullCount += 1
    val last = lastImmutableMap
    val correlId = CorrelId.local()
    val v = keyToValueVersion
    if last != null && last.correlId.eq(correlId) && v == lastKeyToValueVersion then
      last
    else
      lastKeyToValueVersion = v
      newLog4jMapCount += 1
      val r = Log4jMap(correlId)
      lastImmutableMap = r
      r

  // Not used
  def getCopy: java.util.Map[String, String] =
    assert(!isTest)
    getCopyCount += 1
    throw new UnsupportedOperationException("getCopy")


object Log4jThreadContextMap:
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"
  private val myClassName = classOf[Log4jThreadContextMap].getName.stripSuffix("$")
  private var keyToValueVersion = 0

  private[log] val keyToValue = new ConcurrentHashMap[String, String | Lazy[String]]:
    put("js7.version", BuildInfo.longVersion)
    put("js7.longVersion", BuildInfo.longVersion)
    put("js7.prettyVersion", BuildInfo.prettyVersion)
    put("js7.system", Lazy(StartUp.startUpLine))

  //private val dummyNullCorrelId = CorrelId("__NULL__")

  // Counters are not accurate because not synchronized
  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getImmutableMapOrNullCount = 0L
  private var getCopyCount = 0L
  private var newLog4jMapCount = 0L

  private[log] def getOtherKey(key: String): String | Null =
    keyToValue.get(key) match
      case o @ (null | _: String) => o
      case lzy: Lazy[String] => lzy.value

  private val isDebug: Boolean =
    sys.props.asSwitch("log4j2.debug")

  def initialize(name: String): Unit =
    keyToValue.put("js7.serverId", name) // May be overwritten later by a more specific value
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")

  private[log] def set(key: String, value: String): Unit =
    keyToValue.put(key, value)
    keyToValueVersion += 1

  def statistics: String =
    val percent =
      val n = getImmutableMapOrNullCount
      if n == 0 then
        ""
      else
        val a = 100 * newLog4jMapCount / n
        s"($a%)"
    def num(n: Long, name: String) = (n > 0) ? s"$n×$name"
    flatten(
      num(getImmutableMapOrNullCount, "getImmutableMapOrNull"),
      (newLog4jMapCount > 0) ? s"$newLog4jMapCount×$percent new Log4jMap",
      num(Log4jMap.getCount, "Map.get"),
      num(getCopyCount, "getCopy"),
      num(getCount, "get"),
      num(putSuppressedCount, "suppressed")
    ).mkString(", ")

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

  private def debug(string: => String): Unit =
    if isDebug then println(s"$myClassName - $string")
