package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._

/**
 * A <code>Simplifier</code> folds constants and beta-reduces linear variables and/or small arguments.
 */
class Simplifier extends ProgramTransformer {
  def apply(prog: Program): Program = {
    throw new Exception()
  }
}
