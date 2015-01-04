package hdsl.test.parser

import hdsl.test.UnitSpec

import scala.util.parsing.combinator.JavaTokenParsers

import org.junit.Assert._

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

  test("That comments end on newline") {
    val parser = new JavaTokenParsers {
      def mainParser: Parser[List[String]] = rep(comment | ident)
      def comment: Parser[String] = "//.*".r
    }
    val commentThenWord =
      """
        |firstWord
        |// first comment
        |secondWord
        |// second comment
      """.stripMargin
    val parsingResult = parser.parseAll(parser.mainParser, commentThenWord)
    assert(parsingResult.successful)
    assertEquals(List("firstWord", "// first comment", "secondWord", "// second comment"), parsingResult.get)
  }

}
