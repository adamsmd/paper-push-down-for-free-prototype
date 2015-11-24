package org.ucombinator.scheme.cfa.gc

import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.gc.GCInterface

/**
 * @author Ilya Sergey
 */
trait SchemeGarbageCollector extends StateSpace with GCInterface {

  def gc(c: ControlState, frames: Kont): ControlState = c match {
    case ErrorState(_, _) | PFinal(_) => c
    case PState(e, rho, s, kptr) => {
      val alive = reachable(c, frames)
      val cleanStore = s.filter {
        case (a, _) => alive.contains(a)
      }
      PState(e, rho, cleanStore, kptr)
    }
  }

  def reachable(c: ControlState, frames: Kont): Set[Addr] = {
    val rootAddresses: Set[Addr] = rootAddr(c, frames)
    c match {
      case ErrorState(_, _) | PFinal(_) => Set.empty
      case PState(_, _, s, kptr) => {
        val result: Set[Addr] = collectAdjacent(rootAddresses, s)
        result

        if (printGCDebug) {
          val original = (for ((a, _) <- s.bindings) yield a).toSet
          val delta = original -- result
          if (!delta.isEmpty) {
            println("Original store size: " + original.size + "")
            println("Result size: " + result.size + "")
            println("Store delta (size " + delta.size + "):")
            println(delta)
            println()
          }
        }

        result
      }
    }
  }

  /**
   * Get addresses from a stack
   *
   * @param f a frame with environment
   * @return
   */
  def fetchAddressesFromFrame(f: Frame) = f match {
    case LetFrame(_, _, rho) => rho.values
    case IfFrame(_, _, rho) => rho.values
    case SeqFrame(_, _, rho, _) => rho.values
    case _ => Set()
  }

  def collectAdjacent(previousAddrs: Set[Addr], store: Store): Set[Addr] = {

    val filteredStore = store.filter {
      // only addresses in previousAddrs
      case (a, vals) => previousAddrs.contains(a)
    }

    val relevantValues = filteredStore.bindings.flatMap {
      // flatten values
      case (a, vals) => vals
    }

    val relevantClosures = relevantValues.filter {
      // take closures only
      case Clo(lam, rho) => true
      case _ => false
    }.toSet


    val relevantEnvs = relevantClosures.map {
      (v: Val) => v match {
        // take environments
        case Clo(_, rho) => rho
      }
    }
    val newAddresses: Set[Addr] = relevantEnvs.flatMap {
      // take ranges of all environments
      // from closures in store
      rho => rho.values
    }

    if (newAddresses subsetOf previousAddrs) {
      previousAddrs
    } else {
      collectAdjacent(newAddresses ++ previousAddrs, store)
    }
  }

}

