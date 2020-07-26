package js7.base.auth

import js7.base.web.Uri

final case class Admission(uri: Uri, userAndPassword: Option[UserAndPassword])
