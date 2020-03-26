package io.taig.testf

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class AutoTest extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutoTest.Macro.apply
}

private object AutoTest {
  final class Macro(val c: blackbox.Context) {
    def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val (tree, tail) = annottees match {
        case head :: tail => (head.tree, tail.map(_.tree))
        case _            => c.abort(c.enclosingPosition, "Invalid @AutoTest usage")
      }

      val result = tree match {
        case q"$mods class $name[..$types] $ctorMods(...$paramss) extends ..$parents { $self => ..$body }" =>
          val (autoTests, remainingBody) = findAutoTests(c)(body)
          q"""
          $mods class $name[..$types] $ctorMods(...$paramss) extends ..$parents { $self =>
            ..$remainingBody

            ${auto(c)(autoTests)}
          }
          """
        case q"$mods object $name extends ..$parents { $self => ..$body }" =>
          val (autoTests, remainingBody) = findAutoTests(c)(body)

          q"""
          $mods object $name extends ..$parents { $self =>
            ..$remainingBody

            ${auto(c)(autoTests)}
          }
          """
        case _ => c.abort(c.enclosingPosition, "???")
      }

      c.Expr {
        q"""
        $result

        ..$tail
        """
      }
    }

    def auto(c: blackbox.Context)(autoTests: Seq[c.Tree]): c.Tree = {
      import c.universe._

      val impl =
        if (autoTests.isEmpty)
          q"""_root_.io.taig.testf.Test.empty"""
        else
          q"""
      {
        import _root_.cats.implicits._
        _root_.io.taig.testf.Test.allOf[__F, Unit](
          ..${autoTests.map(tree => q"$tree.void")})
      }
      """

      q"""
      override final val auto: _root_.io.taig.testf.Assertion[__F] = $impl
      """
    }

    def findAutoTests(
        c: blackbox.Context
    )(body: Seq[c.Tree]): (Seq[c.Tree], Seq[c.Tree]) = body.partition(_.isTerm)
  }
}
