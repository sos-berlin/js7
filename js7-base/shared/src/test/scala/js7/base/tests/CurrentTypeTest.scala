package js7.base.tests

import js7.base.test.OurTestSuite
import js7.base.tests.CurrentTypeTest.*

/** How to get the current type without F-bounded polymorphism (`trait T[A <: T[A]]`).
  */
final class CurrentTypeTest extends OurTestSuite:

  "test" in:
    assert(typeClass(B()) == B)
    assert(typeClass(C()) == C)
    B().f(): B
    C().f(): C


object CurrentTypeTest:

  private trait Base:
    type This <: Base

    def f(): This

  private trait Base_[A <: Base_[A]] extends Base:
    type This = A

  private object Base_ :
    trait Companion[T <: Base_[T]]:
      given Companion[T] = this



  private final class B extends Base_[B]:
    def f() = this

  private object B extends Base_.Companion[B]


  private final class C extends Base_[C]:
    def f() = this

  private object C extends Base_.Companion[C]


  private def typeClass[A <: Base_[A]: Base_.Companion as A](a: A): Base_.Companion[A] =
    A



  trait Pet {
    // Error Cyclic reference involving val _
    //this: This =>

    type This <: Pet
    def name: String
    def renamed(newName: String): This = this.asInstanceOf[This] // !!
  }

  trait Pet_[T <: Pet]:
    this: T =>
    type This = T
    override def renamed(newName: String): T = this

  case class Fish(name: String, age: Int) extends Pet {
    type This = Fish
    override def renamed(newName: String): Fish = copy(name = newName)
  }

  case class Kitty(name: String, age: Int) extends Pet {
    type This = Kitty
    override def renamed(newName: String): Kitty = copy(name = newName)
  }

  object Test {
    def esquire[A <: Pet](a: A): a.This = a.renamed(a.name + ", Esq.")
    val f: Fish = esquire(new Fish("bob", 22))
  }
