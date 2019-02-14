package com.ayendo.testf

import cats.implicits._
import cats.effect.IO

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

abstract class AutoTestF extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutoTestF.apply
}

private object AutoTestF {
  def apply(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val tree = annottees match {
      case head :: Nil => head.tree
      case _           => c.abort(c.enclosingPosition, "Only objects allowed")
    }

    val result = tree match {
      case q"$mods object $name { $self => ..$body }" =>
        q"""
        $mods object $name extends com.ayendo.testf.TestF { $self =>
          ..$body

          override val suite: List[com.ayendo.testf.Test[cats.effect.IO, Unit]] =
            ${filter(c)(body.toList)}
        }
        """
      case _ => c.abort(c.enclosingPosition, "Only object allowed")
    }

    c.Expr(result)
  }

  def filter(c: blackbox.Context)(
      body: List[c.Tree]): c.Expr[List[Test[IO, Unit]]] = {
    import c.universe._

    val target = typeOf[Test[Nothing, _]].typeSymbol

    val tests = body
      .collect { case field: ValOrDefDef if field.tpt.nonEmpty => field }
      .mapFilter { field =>
        val tpt = c.typecheck(q"??? : ${field.tpt}").tpe.asInstanceOf[TypeRef]
        if (target == tpt.typeSymbol) Some((tpt.args.head, field.name))
        else None

      }
      .map { case (f, term) => q"com.ayendo.testf.LiftIO[$f].lift($term)" }

    val result = q"""
    List(..$tests)
    """

    c.Expr(result)
  }
}
