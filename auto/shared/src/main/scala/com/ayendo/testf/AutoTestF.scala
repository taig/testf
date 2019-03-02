package com.ayendo.testf

import cats.implicits._
import cats.effect.IO

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

abstract class AutoTestF(label: Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutoTestF.apply
}

private object AutoTestF {
  def apply(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val label = c.prefix.tree match {
      case q"new ${_}(label = $label)" =>
        c.eval[Boolean](c.Expr(label))
      case q"new ${_}($label)" =>
        c.eval[Boolean](c.Expr(label))
      case q"new ${_}" => true
    }

    val tree = annottees match {
      case head :: Nil => head.tree
      case _           => c.abort(c.enclosingPosition, "Only objects allowed")
    }

    val result = tree match {
      case q"$mods object $name extends ..$parents { $self => ..$body }" =>
        val head :: tail = parents
        val parent = c.typecheck(q"??? : $head").tpe.asInstanceOf[TypeRef]

        if (parent != typeOf[AnyRef])
          c.abort(c.enclosingPosition, "Invalid parent class")

        q"""
        $mods object $name extends com.ayendo.testf.TestF with ..$tail { $self =>
          ..$body

          override val suite: List[cats.effect.IO[com.ayendo.testf.Test]] =
            ${filter(c)(body.toList, label)}
        }
        """
      case _ => c.abort(c.enclosingPosition, "Only object allowed")
    }

    c.Expr(result)
  }

  def filter(c: blackbox.Context)(body: List[c.Tree],
                                  label: Boolean): c.Expr[List[IO[Test]]] = {
    import c.universe._

    val test = typeOf[Test].typeSymbol

    val tests = body
      .collect { case field: ValOrDefDef if field.tpt.nonEmpty => field }
      .filter {
        case field: DefDef => field.tparams.isEmpty && field.vparamss.isEmpty
        case _: ValDef     => true
      }
      .mapFilter { field =>
        val tpt = c.typecheck(q"??? : ${field.tpt}").tpe.asInstanceOf[TypeRef]
        if (tpt.typeSymbol == test) {
          val test = if (label) autoLabel(c)(field.name) else q"${field.name}"
          Some((tq"cats.Id", test))
        } else if (tpt.args.length == 1 && tpt.args(0).typeSymbol == test) {
          val tq"$name[$_]" = field.tpt
          val test = if (label) autoLabelF(c)(field.name) else q"${field.name}"
          Some((name, test))
        } else None
      }
      .map { case (f, test) => q"com.ayendo.testf.LiftIO[$f].lift($test)" }

    c.Expr(q"List(..$tests)")
  }

  def autoLabel(c: blackbox.Context)(term: c.TermName): c.Tree = {
    import c.universe._
    q"com.ayendo.testf.Test.prefix(${term.decodedName.toString}, $term)"
  }

  def autoLabelF(c: blackbox.Context)(term: c.TermName): c.Tree = {
    import c.universe._
    q"com.ayendo.testf.Test.prefixF(${term.decodedName.toString}, $term)"
  }
}
