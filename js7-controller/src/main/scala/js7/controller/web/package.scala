package js7.controller

import js7.common.akkahttp.web.AkkaWebServer

package object web {
  type ControllerWebServer = AkkaWebServer & AkkaWebServer.HasUri
}
