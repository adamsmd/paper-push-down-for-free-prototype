package org.ucombinator.scheme.cfa.cesk.store

import org.ucombinator.util.MemoizedHashCode

case class MapStore[A, B](map: Map[A, Set[B]]) extends Store[A, B] with MemoizedHashCode {
  def this() = this(Map())

  def get(addr: A): Option[Set[B]] = map get addr

  def wt(that: Store[A, B]) = that match {
    case _ => throw new Exception("Unknown store type")
  }

  /**
  Weak update.
    */
  def +(addr: A, d: Set[B]): MapStore[A, B] = {
    map get addr match {
      case Some(existingD) =>
        new MapStore(map + ((addr, d union existingD)))
      case None => new MapStore(map + ((addr, d)))
    }
  }

  /**
  A simple store does not contain enough information to determine whether a strong update is safe or not, so
   this operation always performs a weak update.
    */
  def update(addr: A, d: Set[B]): Store[A, B] = {
    // We don't have enough information to do a strong update, so we fall back to a weak update.
    this +(addr, d)
  }

  override def toString = "\n " + (map mkString "\n ")

  def toList: List[(A, Set[B])] = map.toList

  def filter(p: ((A, Set[B])) => Boolean): MapStore[A, B] = new MapStore[A, B](map.filter(p))

  def bindings: Iterator[(A, Set[B])] = map.iterator
}
