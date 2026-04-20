package js7.common.metrics

import fs2.{Chunk, Pure}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import js7.base.system.JavaServiceProviders.findJavaService
import js7.data.node.Js7ServerId

object MetricsProvider:

  type ToMetricsStream = () => fs2.Stream[Pure, Chunk[Byte]]

  def toMetricsProvider(ownJs7ServerId: Js7ServerId, configDirectory: Option[Path] = None)
  : ToMetricsStream =
    findJavaService[MetricsJavaService] match
      case None =>
        () =>
          fs2.Stream.emit:
            fs2.Chunk.array:
              s"# $ownJs7ServerId: No MetricsJavaService installed \n"
                .getBytes(UTF_8)

      case Some(svc) =>
        val qServerId = ownJs7ServerId.toString
          .replace("\"", "\\\"")
          .replace("\n", "\\n")
          .replace("\\", "\\\\")
        val addAttribute = s"js7Server=\"$qServerId\""
        () => svc.metricsStreamProvider(configDirectory)(addAttribute = addAttribute)
