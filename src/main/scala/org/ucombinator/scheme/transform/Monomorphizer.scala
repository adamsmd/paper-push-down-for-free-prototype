package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._

/**
 * A <code>Monomorphizer</code> creates a separates definition of every function for each point of use.
 */
class Monomorphizer extends ProgramTransformer {
  def apply(prog: Program): Program = {
    throw new Exception()
  }
}
