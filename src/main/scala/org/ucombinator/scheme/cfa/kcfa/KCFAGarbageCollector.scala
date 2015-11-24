package org.ucombinator.scheme.cfa.kcfa

import org.ucombinator.scheme.cfa.gc.SchemeGarbageCollector

/**
 * @author Ilya Sergey
 */
trait KCFAGarbageCollector extends SchemeGarbageCollector {
  self: PointerCESKMachinery =>

  def rootAddr(c: ControlState, kstore: Kont): Set[Addr] = {
    c match {
      case ErrorState(_, _) | PFinal(_) => Set.empty
      case PState(e, rho, s, kaddr) => {
        val envAddr: Set[Addr] = rho.values.toSet

        val initalkAddr = Set(kaddr)
        val reachableKPtrs: Set[KAddr] = iterateKStore(initalkAddr, kstore)
        val reachableAKonts: Set[AKont] = reachableKPtrs.flatMap(ka => lookupKStore(kstore, ka))

        val reachableFrames: Set[Frame] = reachableAKonts.filter(x => x != MT).map {
          case Pointed(frame, _) => frame
        }

        val stackAddr: Set[Addr] = reachableFrames.flatMap(fetchAddressesFromFrame(_))

        envAddr ++ stackAddr

      }
    }
  }


  /**
   * Garbage collect KStore as well for consistency
   */
  def gcKStore(kaddr: KAddr, kstore: Kont): Kont = {
    val initalkAddr = Set(kaddr)
    val reachableKPtrs: Set[KAddr] = iterateKStore(initalkAddr, kstore)
    kstore.filter {
      case (ka, _) => reachableKPtrs.contains(ka)
    }
  }

  /**
   * get all reachable continuations addresses
   */
  def iterateKStore(workset: Set[KAddr], kstore: KStore): Set[KAddr] = {

    //    val akonts: Set[AKont] = kstore.values.flatten.toSet

    // seems to be incorrect ...

    val akonts: Set[AKont] = workset.flatMap {
      ka => lookupKStore(kstore, ka)
    }

    val newKPtrs: Set[KAddr] = akonts.filter {
      case MT => false
      case _ => true
    }.map {
      case Pointed(frame, kptr) => kptr
    }

    val newWorkSet = workset ++ newKPtrs
    if (newWorkSet == workset) {
      //      println(newWorkSet.size)
      //      println(kstore.keys.size)
      //      println("-----")
      newWorkSet
    } else {
      iterateKStore(newWorkSet, kstore)
    }
  }

}
