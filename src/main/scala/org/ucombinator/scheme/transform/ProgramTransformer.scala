package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._

trait ProgramTransformer {
  def apply(prog: Program): Program;
}
