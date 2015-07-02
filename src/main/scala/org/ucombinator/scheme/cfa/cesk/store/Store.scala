package org.ucombinator.scheme.cfa.cesk.store

trait Store[A, B] {
  def apply(addr: A): Set[B] = (this get addr) match {
    case Some(d) => d
    case None => throw new Exception("Could not find " + addr)
  }

  def getOrElse(addr: A, default: Set[B]): Set[B] = (this get addr) match {
    case Some(d) => d
    case None => default
  }

  def get(addr: A): Option[Set[B]]

  def wt(that: Store[A, B]): Boolean

  /**
  Weak update.
    */
  def +(addr: A, d: Set[B]): Store[A, B]

  /**
  Strong update if safe; weak update otherwise.
    */
  def update(addr: A, d: Set[B]): Store[A, B]

  def filter(p: ((A, Set[B])) => Boolean): Store[A, B]

  def bindings: Iterator[(A, Set[B])]

  def toList: List[(A, Set[B])]
}
