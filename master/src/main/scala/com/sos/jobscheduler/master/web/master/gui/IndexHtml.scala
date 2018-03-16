package com.sos.jobscheduler.master.web.master.gui

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.CheckedOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.BuildInfo.{buildId, buildVersion}
import com.sos.jobscheduler.common.akkahttp.html.HtmlPage
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import scala.language.implicitConversions
import scalatags.Text.all._
import scalatags.Text.{TypedTag, tags2}

/**
  * @author Joacim Zschimmer
  */
object IndexHtml extends HtmlPage.Cached {

  private val logger = Logger(getClass)
  private lazy val resource = JavaResource("com/sos/jobscheduler/master/web/master/gui-js.conf")

  def wholePage: TypedTag[String] =
    html(
      head(
        tags2.title("JobScheduler Master"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0, shrink-to-fit=no"),
        link(rel := "icon", `type` := "image/vnd.microsoft.icon", attr("sizes") := "64x64", `href` := s"master/gui/images/jobscheduler.ico?v=$buildId"),
        link(rel := "stylesheet", href := "master/gui/webjars/materializecss/0.100.2/css/materialize.min.css"),
        link(rel := "stylesheet", href := s"master/gui/gui.css?v=$buildId")),
        body(
          div(id := "GUI")(
            pre("JobScheduler Master...")),
          script(`type` := "text/javascript", src := s"master/gui/master-gui-jsdeps.min.js?v=$buildId"),
          script(`type` := "text/javascript")(s"""
            |jQuery(document).ready(function() {
            |  jQuery('.button-collapse').sideNav();
            |  jQuery('#GUI').on('click', '.clickable-row', function() {
            |    window.location = jQuery(this).data('href');
            |  });
            |});
            |jobschedulerBuildId='$buildId';
            |jobschedulerBuildVersion='$buildVersion';"""
            .stripMargin + "\n"),
          jsName match {
            case Invalid(problem) ⇒
              p(b(color := "red", problem.toString))
            case Valid(o) ⇒
              script(`type` := "text/javascript", src := s"master/gui/$o?v=$buildId")
          }))

  private def jsName: Checked[String] =
    (try Configs.loadResource(resource).optionAs[String]("jsName")
    catch { case t: Throwable ⇒
      logger.error(t.toString, t)
      None
    }) toChecked Problem("Error in JavaResource gui-js.conf")
}
