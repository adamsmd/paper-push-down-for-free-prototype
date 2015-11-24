package org.ucombinator.scheme.scalatest

/**
 * @author Ilya Sergey
 */

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.ucombinator.scheme.parsing.SExpParser

class SExpParsingTest extends FunSpec with ShouldMatchers {

  val p = new SExpParser

  describe("An S-Expression parser") {

    it("should parse simple comments") {
      p.parse(";; Math routines \n (foo)").toString should equal("(foo)")
    }


    it("should parse simple numbers") {
      p.parse("3").toString should equal("3")
    }

    it("should parse simple simple s-expressions") {
      p.parse("()").toString should equal("()")
      p.parse("(3)").toString should equal("(3)")
      p.parse("foo").toString should equal("foo")
      p.parse("( foo bar\n\n\t baz)").toString should equal("(foo bar baz)")
    }

    it("should parse more comments") {
      p.parse("(foo ;bar\n\n baz)").toString should equal("(foo baz)")
    }

    it("should parse spaces") {
      p.parse("(foo )").toString should equal("(foo)")
    }

    it("should other stuff") {
      (p.parseAll(";; Math routines \n ; test \n (foo) (bar) ; baz\n").toString should
        equal("List((foo), (bar))"))
    }

  }

}
