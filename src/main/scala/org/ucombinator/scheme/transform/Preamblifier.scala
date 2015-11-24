package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.parsing.RnRSParser

class Preamblifier extends ProgramTransformer {
  def apply(prog: Program): Program = {
    // Adds the standard preamble.:

    // cons, car, cdr, pair?

    prog match {
      case Program(decs, defs, init) => {
        val p = new RnRSParser

        var newDecs = decs
        var newDefs = defs
        var newInit = init

        newDecs = TypeDec(SName.from("pair"), StrictStruct(List(SName.from("car"), SName.from("cdr")))) :: newDecs

        newDefs = p.parseDef("(define (car p) (struct-get p car pair))") :: newDefs

        newDefs = p.parseDef("(define (cdr p) (struct-get p cdr pair))") :: newDefs

        newDefs = p.parseDef("(define (cons a b) (make-struct pair a b))") :: newDefs

        newDefs = p.parseDef("(define (pair? p) ((type? pair) p))") :: newDefs

        Program(newDecs, newDefs, newInit)
      }
    }
  }
}
