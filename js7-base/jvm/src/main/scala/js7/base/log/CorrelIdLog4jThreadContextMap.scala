package js7.base.log

import java.util.Objects.requireNonNull
import js7.base.BuildInfo
import js7.base.log.CorrelIdLog4jThreadContextMap.*
import js7.base.system.startup.StartUp
import js7.base.utils.Tests.isTest
import org.apache.logging.log4j.spi.{CopyOnWrite, ReadOnlyThreadContextMap, ThreadContextMap}
import org.apache.logging.log4j.util.StringMap

final class CorrelIdLog4jThreadContextMap
extends ThreadContextMap, ReadOnlyThreadContextMap, CopyOnWrite:
  // CopyOnWrite seems to mean that getReadOnlyContextData returns an immutable map.
  // Then Log4j do not make a copy.

  private var lastCorrelIdLog4jStringMap = new CorrelIdLog4jStringMap(CorrelId.empty)

  def clear(): Unit = {}

  def put(key: String, value: String): Unit =
    putSuppressedCount += 1

  def remove(key: String): Unit = {}

  def isEmpty = false

  // Not used
  def get(key: String): String =
    if key == CorrelIdKey then
      getCount += 1
      CorrelId.current.fixedWidthString
    else
      getOtherKey(key)

  // Not used
  def containsKey(key: String): Boolean =
    keys(key)

  // Not used
  def getImmutableMapOrNull: java.util.Map[String, String] =
    getCopy

  // Not used
  def getCopy: java.util.Map[String, String] =
    getCopyCount += 1
    java.util.Collections.singletonMap(CorrelIdKey, CorrelId.local().fixedWidthString)

  def getReadOnlyContextData: StringMap =
    getReadOnlyContextDataCount += 1
    val last = lastCorrelIdLog4jStringMap

    val correlId = CorrelId.local() match
      case null => dummyNullCorrelId  // Happens occasionally in test
      case o => o
    if isTest then requireNonNull(correlId)

    if last.correlId eq correlId then
      last
    else
      getReadOnlyContextDataCount2 += 1
      val r = new CorrelIdLog4jStringMap(correlId)
      lastCorrelIdLog4jStringMap = r
      r


object CorrelIdLog4jThreadContextMap:
  /** Use this name in Log4j2 pattern as `%notEmpty{%X{js7.correlId} }`.
   * The value is empty iff CorrelId are switched off (-Djs7.log.correlId=false). */
  private[log] val CorrelIdKey = "js7.correlId"
  private val NameKey = "js7.name"
  private val VersionKey = "js7.version"
  private val LongVersionKey = "js7.longVersion"
  private val PrettyVersionKey = "js7.prettyVersion"
  private val SystemKey = "js7.system"
  private val keys = Set(CorrelIdKey, NameKey,
    VersionKey, LongVersionKey, PrettyVersionKey, SystemKey)

  private val dummyNullCorrelId = CorrelId("__NULL__")
  private[log] var name = ""

  // Counters are not accurate because not synchronized !!!
  private var putSuppressedCount = 0L
  private var getCount = 0L
  private var getCopyCount = 0L
  private var getReadOnlyContextDataCount = 0L
  private var getReadOnlyContextDataCount2 = 0L

  private[log] def getOtherKey(key: String): String =
    key match
      case NameKey => name
      case VersionKey => BuildInfo.longVersion
      case LongVersionKey => BuildInfo.longVersion
      case PrettyVersionKey => BuildInfo.prettyVersion
      case SystemKey => StartUp.startUpLine()
      case _ => null

  private val isDebug: Boolean =
    sys.props.get("log4j2.debug") match
      case Some("" | "true") => true
      case _ => false

  def initialize(name: String): Unit =
    this.name = name
    System.setProperty("log4j2.threadContextMap", myClassName)
    debug(s"log4j2.threadContextMap=$myClassName")

  def statistics: String =
    val percent =
      val n = getReadOnlyContextDataCount
      if n == 0 then
        ""
      else
        val a = 100 * getReadOnlyContextDataCount2 / n
        s" ($a%)"
    s"$getReadOnlyContextDataCount×$percent getReadOnlyContextData, " +
      s"${CorrelIdLog4jStringMap.forEachCount}× forEach, " +
      s"$getCopyCount× getCopy, " +
      s"$getCount× get, " +
      s"$putSuppressedCount× put (suppressed)"

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

  private def debug(string: => String): Unit =
    if isDebug then println(myClassName + " - " + string)

  private def myClassName =
    classOf[CorrelIdLog4jThreadContextMap].getName.stripSuffix("$")
