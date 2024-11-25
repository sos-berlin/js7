package js7.base.utils

import js7.base.utils.L3.fromBoolean
import org.jetbrains.annotations.TestOnly
import scala.math.Ordering.Implicits.*

/** Łukasiewicz logic Ł3. */
enum L3:
  a =>

  case False, Unknown, True

  //@TestOnly // Correct?
  //infix def xor(b: L3): L3 =
  //  k3xor(b)

  @TestOnly
  infix def k3xor(b: L3): L3 =
    if a == Unknown || b == Unknown then
      Unknown
    else
      fromBoolean(a != b)

  /** Subjunction (implication), K3 Kleene logic. */
  @TestOnly
  infix def k3subjunction(b: => L3): L3 =
    !a || b

  /** Subjunction (implication), Łukasiewicz logic. */
  @TestOnly
  infix def -->(b: => L3): L3 =
    if a == Unknown && b == Unknown then
      True
    else
      !a || b

  def &&(b: => L3): L3 =
    if a == False then
      False
    else
      a & b

  def &(b: L3): L3 =
    a min b

  def ||(b: => L3): L3 =
    if a == True then
      True
    else
      a | b

  def |(b: L3): L3 =
    a max b

  def unary_! : L3 =
    L3.values(2 - ordinal)


object L3:
  given Ordering[L3] = Ordering.by(_.ordinal)

  def fromBoolean(boolean: Boolean): L3 =
    if boolean then True else False
