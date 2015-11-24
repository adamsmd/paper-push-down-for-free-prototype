package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.dsg._

/**
 * Implementation of Introspective pushdown system
 * based on a provided CESK machinery
 *
 * @author Ilya Sergey
 */
trait IPDSMachinery extends StateSpace with PDCFAGarbageCollector {
  self: StackCESKMachinery =>

  type Q = ControlState

  /**
   * Main iteration function of IPDS
   *
   * @param q source control state
   * @param k a [shallow] continuation to make a next step passed instead of a full stack
   * @param framesForGC a set of possible frames in the stack at this state (for Garbage Collection)
   * @return a set of paired control states and stack actions
   */
  def stepIPDS(q: Q, k: List[Frame], framesForGC: List[Frame]): Set[(StackAction[Frame], Q)] = {
    val newQ: Q = (if (shouldGC) gc(q, framesForGC) else q)
    for {
      (q1, k_new) <- mnext(newQ, k)
      g = decideStackAction(k, k_new)
    } yield (g, q1)
  }

  def decideStackAction(k1: List[Frame], k2: List[Frame]): StackAction[Frame] = (k1, k2) match {
    case (x, y) if x == y => Eps
    case (h :: t1, t2) if t1 == t2 => Pop(h)
    case (t1, h :: t2) if t1 == t2 => Push(h)
    case _ => throw new IPDSException("Continuation par is malfomed:\n" +
      "k1: " + k1.toString + "\n" +
      "k2: " + k2.toString)
  }


}


class IPDSException(s: String) extends PDCFAException(s)
