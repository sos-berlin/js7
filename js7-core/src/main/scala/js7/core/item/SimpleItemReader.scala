package js7.core.item

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.=>?
import js7.data.item.{InventoryItem, SimpleItem, SourceType}
import js7.base.io.yaml.YamlExtensions.yamlAs

final class SimpleItemReader private(val companion: SimpleItem.Companion_)
extends ItemReader:

  def read(key: companion.Key, source: ByteArray): SourceType =>? Checked[InventoryItem] =
    case SourceType.Json =>
      source.parseJsonAs(using companion.jsonDecoder)
        .map: item =>
          item.rename(key.asInstanceOf[item.companion.Key])

    case SourceType.Yaml =>
      source.yamlAs(using companion.jsonDecoder)
        .map: item =>
          item.rename(key.asInstanceOf[item.companion.Key])


object SimpleItemReader:
  def apply[I <: SimpleItem: SimpleItem.Companion as I]: SimpleItemReader =
    new SimpleItemReader(I)
