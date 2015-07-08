package org.ucombinator.util

trait MemoizedHashCode {
  self: Product =>
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}
