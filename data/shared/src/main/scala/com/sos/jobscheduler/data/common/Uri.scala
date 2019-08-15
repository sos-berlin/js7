package com.sos.jobscheduler.data.common

import com.sos.jobscheduler.base.generic.GenericString

final case class Uri(string: String) extends GenericString

object Uri extends GenericString.NonEmpty[Uri]
{
  def unchecked(string: String) = new Uri(string)
}
