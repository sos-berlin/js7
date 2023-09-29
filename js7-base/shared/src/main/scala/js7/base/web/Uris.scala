package js7.base.web

import java.lang.Character.isLetter
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.BitSet

/**
  * @author Joacim Zschimmer
  */
object Uris {
  final def encodePath(segments: String*): String =
    segments map encodeSegment mkString "/"

  final def encodeSegment(string: String): String =
    uriSegmentEncoder.encode(string)

  final def encodeQuery(kvs: (String, String)*): String =
    encodeQuery(kvs)

  final def encodeQuery(kvs: Iterable[(String, String)]): String =
    if kvs.isEmpty then
      ""
    else
      "?" + kvs
        .view
        .map { case (k, v) => queryKeyEncoder.encode(k) + "=" + queryValueEncoder.encode(v) }
        .mkString("&")

  private val uriSegmentEncoder = new Encoder(reservedChars = ":/?#[]@")
  private val queryKeyEncoder = new Encoder(reservedChars = "=&#")
  private val queryValueEncoder = new Encoder(reservedChars = "&#")

  private val caseDiff = ('a' - 'A').toChar
  private def allAsciii: Set[Char] = ('\u0021' to '\u007e').toSet

  /* —— TAKEN FROM java.net.URLEncoder ——
   * Copyright (c) 1995, 2021, Oracle and/or its affiliates. All rights reserved.
   * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
   *
   * This code is free software; you can redistribute it and/or modify it
   * under the terms of the GNU General Public License version 2 only, as
   * published by the Free Software Foundation.  Oracle designates this
   * particular file as subject to the "Classpath" exception as provided
   * by Oracle in the LICENSE file that accompanied this code.
   *
   * This code is distributed in the hope that it will be useful, but WITHOUT
   * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   * version 2 for more details (a copy is included in the LICENSE file that
   * accompanied this code).
   *
   * You should have received a copy of the GNU General Public License version
   * 2 along with this work; if not, write to the Free Software Foundation,
   * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
   *
   * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
   * or visit www.oracle.com if you need additional information or have any
   *s questions.
   */
  private final class Encoder(reservedChars: String) {
    private val isAllowedChar = BitSet((allAsciii -- reservedChars.toSet).toSeq.map(_.toInt)*)
    def encode(string: String) =
      if string.forall(c => isAllowedChar(c)) then
        string
      else {
        val result = new StringBuilder(string.length)
        val part = new StringBuilder
        var i = 0

        while i < string.length do {
          var c = string.charAt(i)
          if isAllowedChar(c) then {
            result.append(c)
            i += 1
          } else {
            // convert to UTF-8 before hex conversion
            while i < string.length && { c = string.charAt(i); !isAllowedChar(c)} do {
              part.append(c)
              i += 1
              if c >= 0xD800 && c <= 0xDBFF then {
                // Surrogate pair
                if i < string.length then {
                  val d = string.charAt(i)
                  if d >= 0xDC00 && d <= 0xDFFF then {
                    part.append(d)
                    i += 1
                  }
                }
              }
            }

            val bytes = part.toString.getBytes(UTF_8)
            part.clear()
            for byte <- bytes do {
              result.append('%')
              var ch = Character.forDigit((byte >> 4) & 0xF, 16)
              // converting to use uppercase letter as part of the hex value if ch is a letter.
              if isLetter(ch) then ch = (ch - caseDiff).toChar
              result.append(ch)
              ch = Character.forDigit(byte & 0xF, 16)
              if isLetter(ch) then ch = (ch - caseDiff).toChar
              result.append(ch)
            }
          }
        }

        result.toString
      }
  }
}
