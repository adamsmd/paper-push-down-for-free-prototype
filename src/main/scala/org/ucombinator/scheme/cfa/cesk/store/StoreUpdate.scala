package org.ucombinator.scheme.cfa.cesk.store

case class StoreUpdate[A, B](isStrong: Boolean, addr: A, d: Set[B]) {
  def apply(store: Store[A, B]): Store[A, B] = {
    if (isStrong)
      store(addr) = d
    else
      store +(addr, d)
  }
}
