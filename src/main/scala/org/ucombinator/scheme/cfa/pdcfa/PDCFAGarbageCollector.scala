package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.cfa.gc.SchemeGarbageCollector

/**
 * @author Ilya Sergey
 */
trait PDCFAGarbageCollector extends SchemeGarbageCollector with StackCESKMachinery {

  def rootAddr(c: ControlState, frames: List[Frame]): Set[Addr] = {
    val envAddr: Set[Addr] = c match {
      case ErrorState(_, _) | PFinal(_) => Set.empty
      case PState(e, rho, s, kptr) => rho.values.toSet
    }
    val stackAddr: Set[Addr] = frames.flatMap(fetchAddressesFromFrame(_)).toSet

    envAddr ++ stackAddr
  }

}
