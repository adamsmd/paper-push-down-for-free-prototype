package org.ucombinator.scheme.cfa.cesk.store

case class StoreDelta[A, B](changeLog: List[StoreUpdate[A, B]]) {
  def isEmpty = changeLog.isEmpty

  def apply(store: Store[A, B]): Store[A, B] =
    changeLog.foldLeft(store)((store, update) => update(store))

  def dependencies(): List[A] =
    changeLog.map(_.addr)
}
