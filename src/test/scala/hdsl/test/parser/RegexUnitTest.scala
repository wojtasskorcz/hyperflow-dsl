package hdsl.test.parser

import hdsl.test.UnitSpec

import scala.util.parsing.combinator.JavaTokenParsers

class RegexUnitTest extends UnitSpec {

  test("That curly brackets content is parsed correctly") {
    val parser = new JavaTokenParsers {
      def bracketsParser: Parser[Any] = "{" ~ "[^}]*".r ~ "}"
    }
    val emptyBrackets = "{}"
    val oneLineBrackets =
      """{
        |some text
        |}
      """.stripMargin
    val multilineBrackets =
      """
        |{
        |first line
        |second line
        |}
      """.stripMargin
    val examples = List(emptyBrackets, oneLineBrackets, multilineBrackets)
    for (example <- examples) {
      assert(parser.parseAll(parser.bracketsParser, example).successful)
    }
  }

}
