package com.ayendo.testf

import cats._
import cats.implicits._
import com.ayendo.testf.instance.AllInstances._
import sbt.testing.Status

sealed trait Test extends Product with Serializable {
  def success: Boolean = this match {
    case Test.Error(_) | Test.Failure(_) => false
    case Test.Group(tests)               => tests.forall(_.success)
    case Test.Label(_, test)             => test.success
    case Test.Message(_, test)           => test.success
    case Test.Success | Test.Skip(_)     => true
  }

  def error: Boolean = this match {
    case Test.Error(_)                                 => true
    case Test.Failure(_) | Test.Success | Test.Skip(_) => false
    case Test.Group(tests)                             => tests.exists(_.error)
    case Test.Label(_, test)                           => test.error
    case Test.Message(_, test)                         => test.error
  }

  def failure: Option[Throwable] = this match {
    case Test.Error(_) | Test.Success | Test.Skip(_) => None
    case Test.Failure(throwable)                     => Some(throwable)
    case Test.Group(tests)                           => tests.collectFirstSome(_.failure)
    case Test.Label(_, test)                         => test.failure
    case Test.Message(_, test)                       => test.failure
  }

  def status: Status = this match {
    case Test.Error(_)         => Status.Error
    case Test.Failure(_)       => Status.Failure
    case Test.Group(tests)     => tests.map(_.status).combineAll
    case Test.Label(_, test)   => test.status
    case Test.Message(_, test) => test.status
    case Test.Skip(_)          => Status.Skipped
    case Test.Success          => Status.Success
  }
}

object Test extends TestBuilders {
  final case class Error(message: String) extends Test

  final case class Failure(throwable: Throwable) extends Test

  final case class Group(tests: List[Test]) extends Test

  final case class Label(description: String, test: Test) extends Test

  final case class Message(description: String, test: Test) extends Test

  final case class Skip(test: Test) extends Test

  final case object Success extends Test

  implicit val semigroup: Semigroup[Test] = {
    case (Group(xs), Group(ys)) => Group(xs |+| ys)
    case (Group(xs), y)         => Group(xs :+ y)
    case (x, Group(ys))         => Group(x +: ys)
    case (x, y)                 => Group(List(x, y))
  }

  implicit val eq: Eq[Test] =
    new Eq[Test] {
      override def eqv(x: Test, y: Test): Boolean = {
        PartialFunction.cond((x, y)) {
          case (Error(m1), Error(m2))           => m1 === m2
          case (Failure(t1), Failure(t2))       => t1 == t2
          case (Group(xs), Group(ys))           => xs === ys
          case (Label(dx, x), Label(dy, y))     => dx === dy && eqv(x, y)
          case (Message(dx, x), Message(dy, y)) => dx === dy && eqv(x, y)
          case (Skip(x), Skip(y))               => eqv(x, y)
          case (Success, Success)               => true
        }
      }
    }

  implicit val show: Show[Test] =
    new Show[Test] {
      override def show(test: Test): String = test match {
        case Error(message)           => s"Error($message)"
        case Failure(throwable)       => s"Failure(${throwable.getMessage})"
        case Group(tests)             => s"Group(${tests.map(show).mkString(", ")})"
        case Label(description, test) => s"Label($description, ${show(test)})"
        case Message(description, test) =>
          s"Message($description, ${show(test)})"
        case Skip(test) => s"Skip(${show(test)})"
        case Success    => s"Success"
      }
    }
}
