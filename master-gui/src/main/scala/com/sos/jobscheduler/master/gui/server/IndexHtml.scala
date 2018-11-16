package com.sos.jobscheduler.master.gui.server

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.problem.Checked.CheckedOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.BuildInfo.{buildId, buildVersion}
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.gui.server.common.HtmlPage
import com.typesafe.config.Config
import io.circe.Json
import io.circe.syntax.EncoderOps
import scala.language.implicitConversions
import scalatags.Text.all._
import scalatags.Text.{TypedTag, tags2}

/**
  * @author Joacim Zschimmer
  */
final class IndexHtml(config: Config) extends HtmlPage.Cached {

  private val logger = Logger(getClass)
  private lazy val resource = JavaResource("com/sos/jobscheduler/master/gui/server/gui-js.conf")

  def wholePage: TypedTag[String] =
    html(
      head(
        tags2.title("JobScheduler Master"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0, shrink-to-fit=no"),
        link(rel := "icon", `type` := "image/vnd.microsoft.icon", attr("sizes") := "64x64", `href` := s"master/gui/images/jobscheduler.ico?v=$buildId"),
        link(rel := "stylesheet", href := "master/gui/webjars/bootstrap/4.0.0/dist/css/bootstrap.min.css"),
        link(rel := "stylesheet", href := s"master/gui/gui.css?v=$buildId")),
        body(
          div(id := "GUI")(
            pre("JobScheduler Master...")),
          script(`type` := "text/javascript", src := s"master/gui/master-gui-browser-jsdeps.min.js?v=$buildId"),
          script(`type` := "text/javascript")(raw(s"""
           |guiConfig=${guiConfig.pretty(CompactPrinter)};
           |jQuery(document).ready(function() {
           |  jQuery('#GUI').on('click', '.clickable-row', function() {
           |    window.location = jQuery(this).data('href');
           |  });
           |});""".stripMargin + "\n")),
          jsName match {
            case Invalid(problem) ⇒
              p(b(color := "red", problem.toString))
            case Valid(o) ⇒
              script(`type` := "text/javascript", src := s"master/gui/$o?v=$buildId")
          }))

  private def guiConfig = Json.obj(
    "buildId"      → buildId.asJson,
    "buildVersion" → buildVersion.asJson,
    "tornOlderSeconds" → config.getDuration("jobscheduler.gui.torn-older").getSeconds.toInt.asJson)
  private def jsName: Checked[String] =
    (try Configs.loadResource(resource).optionAs[String]("jsName")
    catch { case t: Throwable ⇒
      logger.error(t.toString, t)
      None
    }) toChecked Problem("Error in JavaResource gui-js.conf")
}
