package com.ayendo.testf

import cats._
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.Test.{Assert, Group, Label}

sealed trait Test extends Product with Serializable {
  def run: IO[Summary] = this match {
    case Assert(description, result) =>
      result.map {
        case Result.Success        => Summary.Success(description)
        case Result.Error(message) => Summary.Error(description, message)
      }
    case Group(left, right) =>
      (left.run, right.run).mapN { (left, right) =>
        Summary.Group(left, right, description = None)
      }
    case Label(description, Assert(_, result)) =>
      Assert(description, result).run
    case Label(description, Group(left, right)) =>
      (left.run, right.run).mapN { (left, right) =>
        Summary.Group(left, right, description = Some(description))
      }
    case Label(description, Label(_, test)) =>
      Label(description, test).run
  }
}

object Test {
  case class Assert(description: String, result: IO[Result]) extends Test

  case class Group(left: Test, right: Test) extends Test

  case class Label(description: String, test: Test) extends Test

  implicit val semigroup: Semigroup[Test] = Group.apply
}
