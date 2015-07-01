package org.ucombinator.scheme.cfa.cesk.store

trait LoggingStore[A, B] extends Store[A, B] {

  def changeLog: StoreDelta[A, B]

  def resetChangeLog: LoggingStore[A, B]

}
