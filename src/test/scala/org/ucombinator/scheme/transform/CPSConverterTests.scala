package org.ucombinator.scheme.transform

import org.ucombinator.scheme.parsing.{RnRSParser, SExpParser}

object CPSConverterTests {

  def main(args: Array[String]) {
    val sxp = new SExpParser
    val p = new RnRSParser
    val t = new ANormalizer
    val c = new CPSConverter

    if (args.length > 0) {

      val filename = args(0)

      System.err.println("CPS-converting: " + filename)

      val lines = scala.io.Source.fromFile(filename).mkString("")

      val sexps = sxp.parseAll(lines)
      val ast = p.parseProgram(sexps)
      val anast = t.apply(ast)
      val cpast = c.apply(anast)

      println(cpast)
    }

  }
}
