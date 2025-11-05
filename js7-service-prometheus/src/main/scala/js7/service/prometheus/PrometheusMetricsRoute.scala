package js7.service.prometheus

import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.CommonConfiguration
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpResponse}
import org.apache.pekko.http.scaladsl.server.Directives.{complete, get, pathEnd}
import org.apache.pekko.http.scaladsl.server.Route

trait PrometheusMetricsRoute:

  protected val conf: CommonConfiguration

  private lazy val prometheusAdapter =
    new PrometheusJmxAdapter(Some(conf.configDirectory))

  /** /metrics web service according to Prometheus.
    * <p>
    *   Prometheus expects a web service path "/metrics".
    *
    * @see https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
    * @see https://prometheus.io/docs/practices/naming/
    */
  final val metricsRoute: Route =
    (pathEnd & get):
      complete:
        HttpResponse(
          entity = HttpEntity(`text/plain(UTF-8)`, prometheusAdapter.metricsByteString()))
