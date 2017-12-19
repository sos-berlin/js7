package com.sos.jobscheduler.master.web.master.gui

import com.sos.jobscheduler.common.BuildInfo.{buildId, buildVersion}
import com.sos.jobscheduler.common.akkahttp.html.HtmlPage
import scala.language.implicitConversions
import scalatags.Text.all._
import scalatags.Text.{TypedTag, tags2}

/**
  * @author Joacim Zschimmer
  */
object IndexHtml extends HtmlPage {

  val wholePage: TypedTag[String] =
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
        script(`type` := "text/javascript")(
          "$('.button-collapse').sideNav();" +  // Materialize CSS
          s"jobschedulerBuildId='$buildId';" +
          s"jobschedulerBuildVersion='$buildVersion';"),
        script(`type` := "text/javascript", src := s"master/gui/master-gui.js?v=$buildId")))
}
