object ScalacheckGenerator {
  def apply(pkg: String, name: String): String = {
    s"""package $pkg
       |
       |import org.scalacheck.Test.Parameters
       |import org.scalacheck.util.Pretty
       |import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
       |
       |trait $name {
       |${(1 to 8).map(check).mkString("\n")}
       |}
     """.stripMargin
  }

  def types(length: Int): String =
    (1 to length).map(index => s"A$index").mkString(", ")

  def arbitraryTypes(length: Int): String =
    (1 to length).map(index => s"A$index: Arbitrary").mkString(", ")

  def parameters(length: Int): String =
    (1 to length).map(index => s"a$index: Gen[A$index]").mkString(", ")

  def implicits(length: Int): String =
    (1 to length)
      .map(index => s"s$index: Shrink[A$index], pp$index: A$index => Pretty")
      .mkString(", ")

  def argumentsGen(length: Int): String =
    (1 to length).map(index => s"a$index").mkString(", ")

  def check(length: Int): String = {
    s"""  def check$length[${types(length)}](${parameters(length)}, parameters: Parameters)(f: (${types(
         length
       )}) => Assertion[Pure])(
       |    implicit
       |    ${implicits(length)}
       |  ): Assertion[Pure] =
       |    ScalacheckAssertions.checkTest(
       |      implicit prop => Prop.forAll(${argumentsGen(length)})(f),
       |      parameters
       |    )
       |
       |  def check$length[${types(length)}](${parameters(length)})(f: (${types(
         length
       )}) => Assertion[Pure])(
       |    implicit
       |    ${implicits(length)}
       |  ): Assertion[Pure] = check$length(${argumentsGen(length)}, Parameters.default)(f)
       |
       |  def check$length[${arbitraryTypes(length)}](parameters: Parameters)(f: (${types(
         length
       )}) => Assertion[Pure])(
       |    implicit
       |    ${implicits(length)}
       |  ): Assertion[Pure] =
       |    ScalacheckAssertions.checkTest(
       |      implicit prop => Prop.forAll(f),
       |      parameters
       |    )
       |
       |    def check$length[${arbitraryTypes(length)}](f: (${types(length)}) => Assertion[Pure])(
       |    implicit
       |    ${implicits(length)}
       |  ): Assertion[Pure] =
       |    check$length(Parameters.default)(f)
     """.stripMargin
  }
}
