package org.ucombinator.scheme.cfa.cesk.store

class SentinelStore[A, B](updates: List[StoreUpdate[A, B]], val store: Store[A, B]) extends LoggingStore[A, B] {

  def this(store: Store[A, B]) = this(List(), store)

  def get(addr: A) = store get addr

  def wt(that: Store[A, B]) = that match {
    case thatStore: SentinelStore[A, B] => store wt thatStore.store
    case _ => store wt that
  }

  def +(addr: A, d: Set[B]): SentinelStore[A, B] = (store get addr) match {
    case Some(d2) if (d subsetOf d2) => this
    case _ => new SentinelStore(StoreUpdate(false, addr, d) :: updates, store +(addr, d))
  }

  def update(addr: A, d: Set[B]): SentinelStore[A, B] = (store get addr) match {
    case Some(d2) if (d subsetOf d2) => this
    case _ => new SentinelStore(StoreUpdate[A, B](true, addr, d) :: updates, store(addr) = d)
  }

  def changeLog: StoreDelta[A, B] = StoreDelta[A, B](updates)

  def resetChangeLog: LoggingStore[A, B] = new SentinelStore(store)

  override def toString = store.toString

  def toList = store.toList

  def filter(p: ((A, Set[B])) => Boolean): SentinelStore[A, B] = new SentinelStore[A, B](updates, store.filter(p))

  def bindings: Iterator[(A, Set[B])] = store.bindings
}
