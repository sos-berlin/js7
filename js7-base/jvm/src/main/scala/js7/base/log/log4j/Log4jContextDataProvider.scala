package js7.base.log.log4j

import java.util.concurrent.ConcurrentHashMap
import java.util.{HashMap as JHashMap, Map as JMap}
import js7.base.BuildInfo
import js7.base.log.Logger
import js7.base.log.log4j.Log4jContextDataProvider.*
import js7.base.system.startup.StartUp
import js7.base.utils.ScalaUtils.flatten
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Lazy, ScalaUtils}
import org.apache.logging.log4j.core.util.ContextDataProvider
import org.apache.logging.log4j.util.{SortedArrayStringMap, StringMap}
import scala.jdk.CollectionConverters.*

final class Log4jContextDataProvider extends ContextDataProvider:

  def supplyContextData: JMap[String, String] =
    asJavaMap

  // Not used
  override def supplyStringMap: StringMap =
    asStringMap


object Log4jContextDataProvider:

  private[log] val keyToValue = new ConcurrentHashMap[String, String | Lazy[String]]:
    this.put("js7.version", BuildInfo.longVersion)
    this.put("js7.longVersion", BuildInfo.longVersion)
    this.put("js7.prettyVersion", BuildInfo.prettyVersion)
    this.put("js7.system", Lazy.fast(StartUp.startUpLine))
    //this.put(CorrelIdKey, "❓init❓") // Placeholder in SortedArrayStringMap for fast override

  private var keyToValueVersion = 0
  private var _javaMap: JMap[String, String] = null.asInstanceOf[JMap[String, String]]
  private var _javaMapVersion = -1
  private var _javaMapCount = 0
  private var _stringMap: StringMap = null.asInstanceOf[StringMap]
  private var _stringMapVersion = -1
  private var _stringMapMapCount = 0

  def initialize(name: String): Unit =
    keyToValue.put("js7.serverId", name) // May be overwritten later by a more specific value

  private[log] def put(key: String, value: String): Unit =
    keyToValue.put(key, value)
    keyToValueVersion += 1

  /*private*/ inline def asJavaMap: JMap[String, String] =
    if _javaMapVersion != keyToValueVersion then
      // Copying to a new HashMap is fast when it is merged with another HashMap.
      _javaMap = new JHashMap(keyToValue.asScala.view.mapValues(resolveValue).toMap.asJava)
      _javaMapVersion = keyToValueVersion
      _javaMapCount += 1
    _javaMap

  private /*inline*/ def asStringMap: StringMap =
    if _stringMapVersion != keyToValueVersion then
      // SortedArrayStringMap is fast when it is merged with another SortedArrayStringMap.
      _stringMap = SortedArrayStringMap:
        keyToValue.asScala.view.mapValues(resolveValue).toMap.asJava
      _stringMapVersion = keyToValueVersion
      _stringMapMapCount += 1
    _stringMap

  private def resolveValue(value: String | Null | Lazy[String]): String =
    value match
      case lzy: Lazy[String] => lzy.value
      case o => o.asInstanceOf[String]

  def logStatistics(): Unit =
    Logger[this.type].trace(statistics)

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
      num(_javaMapCount, "javaMap"),
      num(_stringMapMapCount, "stringMap"),
    ).mkString(", ")
