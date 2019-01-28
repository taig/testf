package com.ayendo.testf.scalacheck

import cats.Id
import com.ayendo.testf._
import org.scalacheck.{Gen, Prop, Shrink}
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders {
  def check(
      description: String,
      prop: Prop,
      parameters: Parameters = Parameters.default): Test[Id, Assertion] = {
    val result = org.scalacheck.Test.check(parameters, prop)

    if (result.passed) success(description)
    else error(description, Pretty.pretty(result, Pretty.Params(2)))
  }

  def check1[A, P](
      description: String,
      parameters: Parameters = Parameters.default)(a: Gen[A])(f: A => P)(
      implicit p: P => Prop,
      sa: Shrink[A],
      ppa: A => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a)(f), parameters)

  def check2[A, B, P](
      description: String,
      parameters: Parameters = Parameters.default)(a: Gen[A], b: Gen[B])(
      prop: (A, B) => P)(implicit p: P => Prop,
                         sa: Shrink[A],
                         sb: Shrink[B],
                         ppa: A => Pretty,
                         ppb: B => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b)(prop), parameters)

  def check3[A, B, C, P](description: String,
                         parameters: Parameters = Parameters.default)(
      a: Gen[A],
      b: Gen[B],
      c: Gen[C])(prop: (A, B, C) => P)(implicit p: P => Prop,
                                       sa: Shrink[A],
                                       sb: Shrink[B],
                                       sc: Shrink[C],
                                       ppa: A => Pretty,
                                       ppb: B => Pretty,
                                       ppc: C => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b, c)(prop), parameters)

  def check4[A, B, C, D, P](description: String,
                            parameters: Parameters = Parameters.default)(
      a: Gen[A],
      b: Gen[B],
      c: Gen[C],
      d: Gen[D])(prop: (A, B, C, D) => P)(
      implicit p: P => Prop,
      sa: Shrink[A],
      sb: Shrink[B],
      sc: Shrink[C],
      sd: Shrink[D],
      ppa: A => Pretty,
      ppb: B => Pretty,
      ppc: C => Pretty,
      ppd: D => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b, c, d)(prop), parameters)

  def check5[A, B, C, D, E, P](description: String,
                               parameters: Parameters = Parameters.default)(
      a: Gen[A],
      b: Gen[B],
      c: Gen[C],
      d: Gen[D],
      e: Gen[E])(prop: (A, B, C, D, E) => P)(
      implicit p: P => Prop,
      sa: Shrink[A],
      sb: Shrink[B],
      sc: Shrink[C],
      sd: Shrink[D],
      se: Shrink[E],
      ppa: A => Pretty,
      ppb: B => Pretty,
      ppc: C => Pretty,
      ppd: D => Pretty,
      ppe: E => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b, c, d, e)(prop), parameters)

  def check6[A, B, C, D, E, F, P](description: String,
                                  parameters: Parameters = Parameters.default)(
      a: Gen[A],
      b: Gen[B],
      c: Gen[C],
      d: Gen[D],
      e: Gen[E],
      f: Gen[F])(prop: (A, B, C, D, E, F) => P)(
      implicit p: P => Prop,
      sa: Shrink[A],
      sb: Shrink[B],
      sc: Shrink[C],
      sd: Shrink[D],
      se: Shrink[E],
      sf: Shrink[F],
      ppa: A => Pretty,
      ppb: B => Pretty,
      ppc: C => Pretty,
      ppd: D => Pretty,
      ppe: E => Pretty,
      ppf: F => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b, c, d, e, f)(prop), parameters)

  def check7[A, B, C, D, E, F, G, P](
      description: String,
      parameters: Parameters = Parameters.default)(
      a: Gen[A],
      b: Gen[B],
      c: Gen[C],
      d: Gen[D],
      e: Gen[E],
      f: Gen[F],
      g: Gen[G])(prop: (A, B, C, D, E, F, G) => P)(
      implicit p: P => Prop,
      sa: Shrink[A],
      sb: Shrink[B],
      sc: Shrink[C],
      sd: Shrink[D],
      se: Shrink[E],
      sf: Shrink[F],
      sg: Shrink[G],
      ppa: A => Pretty,
      ppb: B => Pretty,
      ppc: C => Pretty,
      ppd: D => Pretty,
      ppe: E => Pretty,
      ppf: F => Pretty,
      ppg: G => Pretty): Test[Id, Assertion] =
    check(description, Prop.forAll(a, b, c, d, e, f, g)(prop), parameters)
}
