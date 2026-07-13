package js7.base.log.log4j

import java.util.Map as JMap
import java.util.concurrent.ConcurrentHashMap
import js7.base.BuildInfo
import js7.base.log.Logger
import js7.base.log.log4j.Log4jThreadContextMap.*
import js7.base.system.startup.StartUp
import js7.base.utils.ScalaUtils.flatten
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SystemPropertiesExtensions.asSwitch
import js7.base.utils.Tests.isTest
import js7.base.utils.{Lazy, ScalaUtils}
import org.apache.logging.log4j.spi.{ReadOnlyThreadContextMap, ThreadContextMap}
import org.apache.logging.log4j.util.SortedArrayStringMap
import scala.jdk.CollectionConverters.*

final class Log4jThreadContextMap extends ThreadContextMap, ReadOnlyThreadContextMap:

  private var lastKeyToValueVersion = keyToValueVersion - 1

  def clear(): Unit =
    putSuppressedCount += 1

  def put(key: String, value: String): Unit =
    putSuppressedCount += 1

  def remove(key: String): Unit =
    putSuppressedCount += 1
    assert(!isTest)

  def isEmpty =
    asStringMap.isEmpty

  // Not used
  def get(key: String): String | Null =
    getCount += 1
    assert(!isTest)
    asStringMap.getValue(key)

  // Not used
  def containsKey(key: String): Boolean =
    assert(!isTest)
    asStringMap.containsKey(key)

  // ReadOnlyThreadContextMap:
  override def getReadOnlyContextData =
    getReadOnlyContextDataCount += 1
    asStringMap

  // Not used because we implement faster ReadOnlyThreadContextMap
  def getImmutableMapOrNull: JMap[String, String] =
    getImmutableMapOrNullCount += 1
    asStringMap.toMap

  // Not used
  def getCopy: JMap[String, String] =
    getCopyCount += 1
    asStringMap.toMap


object Log4jThreadContextMap:
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"
  private val myClassName = classOf[Log4jThreadContextMap].getName.stripSuffix("$")
  private val isDebug = sys.props.asSwitch("log4j2.debug")

  private[log] val keyToValue = new ConcurrentHashMap[String, String | Lazy[String]]:
    put("js7.version", BuildInfo.longVersion)
    put("js7.longVersion", BuildInfo.longVersion)
    put("js7.prettyVersion", BuildInfo.prettyVersion)
    put("js7.system", Lazy.fast(StartUp.startUpLine))
    //put(CorrelIdKey, "❓init❓") // Placeholder in SortedArrayStringMap for fast override

  private var keyToValueVersion = 0
  private var _stringMap: SortedArrayStringMap = null.asInstanceOf[SortedArrayStringMap]
  private var _stringMapVersion = -1

  // Counters are not accurate because not synchronized
  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getReadOnlyContextDataCount = 0L
  private var getImmutableMapOrNullCount = 0L
  private var getCopyCount = 0L

  def initialize(name: String): Unit =
    keyToValue.put("js7.serverId", name) // May be overwritten later by a more specific value
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")

  private[log] def set(key: String, value: String): Unit =
    keyToValue.put(key, value)
    keyToValueVersion += 1

  private inline def asStringMap: SortedArrayStringMap =
    if keyToValueVersion != _stringMapVersion then
      makeStringMap()
    _stringMap

  private def makeStringMap(): Unit =
    // SortedArrayStringMap is fast when it is merged with another SortedArrayStringMap.
    _stringMap = new SortedArrayStringMap(
      keyToValue.asScala.view.mapValues(resolveValue).toMap.asJava)
    _stringMapVersion = keyToValueVersion

  private def resolveValue(value: String | Lazy[String] | Null): String | Null =
    value match
      case o @ (null | _: String) => o
      case lzy: Lazy[String] => lzy.value

  def statistics: String =
    //val percent =
    //  val n = getReadOnlyContextDataCount
    //  if n == 0 then
    //    ""
    //  else
    //    val a = 100 * newLog4jMapCount / n
    //    s"($a%)"
    def num(n: Long, name: String) = (n > 0) ? s"$n×$name"
    flatten(
      num(getReadOnlyContextDataCount, "getReadOnlyContextData"),
      //(newLog4jMapCount > 0) ? s"$newLog4jMapCount×$percent new Log4jMap",
      num(getImmutableMapOrNullCount, "getImmutableMapOrNull"),
      num(getCopyCount, "getCopy"),
      num(getCount, "get"),
      num(putSuppressedCount, "suppressed")
    ).mkString(", ")

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

  private def debug(string: => String): Unit =
    if isDebug then println(s"$myClassName - $string")
