/*
 * CRAPL 2012.
 * U Combinator, University of Utah
 * DistriNet, KU Leuven
 *
 * THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
 * APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
 * HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT
 * WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
 * DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
 * CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
 * WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
 * CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
 * INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
 * NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
 * LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM
 * TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
 * PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * If you have questions or concerns about the CRAPL, or you need more
 * information about this license, please contact:
 *
 *    Matthew Might
 *    http://matt.might.net/
 */

package org.ucombinator.scheme.cfa.kcfa

import org.ucombinator.scheme.cfa.cesk.store.{Store => StoreInterface, LoggingStore, MapStore, SentinelStore}
import org.ucombinator.scheme.syntax._
import org.ucombinator.util.FancyOutput
import org.ucombinator.cfa.AnalysisRunner
import org.ucombinator.scheme.cfa.cesk.CESKMachinery

/**
 * @author ilya
 */

trait PointerCESKMachinery extends CESKMachinery with FancyOutput {
  self: AnalysisRunner with KCFAGarbageCollector =>

  type Kont = KStore

  type Addr = (Var, List[Exp])

  /** ******************************************************************
    * Continuation sotre
    * *******************************************************************/
  type KAddr = (Exp, Env)

  type KStore = StoreInterface[KAddr, AKont]

  /** ******************************************************************
    * Continuations with pointers
    * *******************************************************************/
  abstract sealed class AKont

  object MT extends AKont

  case class Pointed(frame: Frame, kptr: KAddr) extends AKont


  /** ******************************************************************
    * Utility functions
    * *******************************************************************/
  def alloc(v: Var, c: Conf): Addr = c match {
    case (PState(e, _, _, kptr), _) =>
      if (isDummy) {
        (SName("SingleAddr", 0), Nil)
      } else k match {
        case 0 => (v, Nil)
        case 1 => (v, List(e))
        case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => {
      throw new PointerCESKException("Illegal allocation configuartion:\n" + c.toString)
    }
  }

  def initState(e: Exp): Conf = {
    val a0: KAddr = (new Unspecified(), Map.empty)
    val newKStore: KStore = (new MapStore[KAddr, AKont]() + (a0, Set(MT)))
    (PState(e, Map.empty, new MapStore(Map.empty), a0), newKStore)
  }

  def kalloc(c: Conf, exp: Exp, env: Env): KAddr = (exp, env)

  def lookupKStore(kstore: KStore, a: KAddr): Set[AKont] = kstore.get(a) match {
    case Some(x) => x
    case None => throw new PointerCESKException("No associated continuations found for an address " +
      a + "\nin store\n" + kstore.toString)
  }

  def updateKStore(kstore: KStore, pair: (KAddr, AKont)) = {
    val (a, k) = pair
    val oldKonts = kstore.getOrElse(a, Set())
    kstore + (a, oldKonts + k)
  }

  class PointerCESKException(s: String) extends CESKException(s)

  /** ******************************************************************
    * Main non-deterministic abstract step function
    * *******************************************************************/
  def mnext: Conf => Set[Conf] = {

    // (let ([x V]) E)
    case c@(PState(LetForm(v, value, body), env, store, kaddr), kstore) if isAtomic(value) => {
      val a = alloc(v, c)
      val env1 = updateEnv(env, v, a)
      val store1 = updateStore(store, a, atomicEval(value, env, store))
      Set((PState(body, env1, store1, kaddr), kstore))
    }

    // (let ([x (if V E E)]) E)
    case c@(PState(e@LetForm(v, i@If(cond, tBranch, eBranch), body), env, store, kaddr), kstore) => {
      val frame = LetFrame(v, body, env)
      val condValues = atomicEval(cond, env, store)
      var succs = Set[Conf]()
      if (condValues.exists(_ != BoolLit(false))) {
        val kaddr1 = kalloc(c, tBranch, env)
        val kstore1 = updateKStore(kstore, (kaddr1, Pointed(frame, kaddr)))
        succs = succs + ((PState(tBranch, env, store, kaddr1), kstore1))
      }
      if (condValues.contains(BoolLit(false))) {
        val kaddr1 = kalloc(c, eBranch, env)
        val kstore1 = updateKStore(kstore, (kaddr1, Pointed(frame, kaddr)))
        succs = succs + ((PState(e, env, store, kaddr1), kstore1))
      }
      succs
    }

    // (let ([x (set! x V)]) E)
    case (c@PState(LetForm(_, SetVar(v, ae), body), env, store, kaddr), kstore) => {
      val addr = lookupEnv(env, v)
      val values = atomicEval(ae, env, store)
      val store1 = updateStore(store, addr, values)
      Set((PState(body, env, store1, kaddr), kstore))
    }

    // (let ([x (V V ...)]) E)
    case c@(PState(e@LetForm(v, a@AppForm(f, args), body), env, store, kaddr), kstore) => {
      val frame = LetFrame(v, body, env)
      atomicEval(f, env, store).flatMap[Conf, Set[Conf]] {
        case Clo(LambdaForm(params, body), env1) => {
          val ai = params.map(alloc(_, c))
          val env2 = updateEnv(env1, params.zip(ai))

          val argVals = args.map(atomicEval(_, env, store))
          val store1 = updateStore(store, ai.zip(argVals))

          val kaddr1 = kalloc(c, body, env2)
          val kstore1 = updateKStore(kstore, (kaddr1, Pointed(frame, kaddr)))

          Set((PState(body, env2, store1, kaddr1), kstore1))
        }
      }
    }

    // (if V E E)
    case (PState(If(cond, tBranch, eBranch), env, store, kaddr), kstore) => {
      val condValues = atomicEval(cond, env, store)
      var succs = Set[Conf]()
      if (condValues.exists(_ != BoolLit(false)))
        succs = succs + ((PState(tBranch, env, store, kaddr), kstore))
      if (condValues.contains(BoolLit(false)))
        succs = succs + ((PState(eBranch, env, store, kaddr), kstore))
      succs
    }

    // (set! x V)
    case c@(PState(SetVar(v, ae), env, store, kaddr), kstore) => {
      val addr = lookupEnv(env, v)
      val values = atomicEval(ae, env, store)
      val store1 = updateStore(store, addr, values)
      for {
        k <- lookupKStore(kstore, kaddr)
        next <- returnValue(k, Set(), kstore, c, store1)
      } yield next
    }

    // ae
    case c@(PState(ae, env, store, kaddr), kstore) if isAtomic(ae) => {
      for {
        k <- lookupKStore(kstore, kaddr)
        values = atomicEval(ae, env, store)
        next <- returnValue(k, values, kstore, c, store)
      } yield next
    }

    // (V V ...)
    case c@(PState(e@AppForm(f, args), env, store, kaddr), kstore) => {
      atomicEval(f, env, store).flatMap[Conf, Set[Conf]] {
        case Clo(LambdaForm(params, body), env1) => {
          val ai = params.map(alloc(_, c))
          val env2 = updateEnv(env1, params.zip(ai))

          val argVals = args.map(atomicEval(_, env, store))
          val store1 = updateStore(store, ai.zip(argVals))

          val kaddr1 = kalloc(c, body, env2)
          val kstore1 = kstore + (kaddr1, kstore(kaddr))

          Set((PState(body, env2, store1, kaddr1), kstore1))
        }
      }
    }

    /** ****************************************************
      * Final state
      * *****************************************************/
    // Ok, folks, that's it!
    case (PFinal(_), _) => Set()

    case c => {
      throw new PointerCESKException("Wrong state: " + c.toString)
    }
  }

  /**
   * Value return
   */
  def returnValue(k: AKont, values: Set[Val], kstore: KStore, c: Conf,  s: Store): Set[Conf] = k match {
    // return to final state
    case MT => Set((PFinal(values), kstore))

    // return from let-statement
    case Pointed(LetFrame(v, e, env), b) => {
      val a = alloc(v, c)
      val rho2 = updateEnv(env, List((v, a)))
      val s1 = updateStore(s, List((a, values)))
      Set((PState(e, rho2, s1, b), kstore))
    }
  }

  def addressOf(exp: Exp, env: Env) : Option[Addr] = exp match {
    case Ref(name) => Some(env(name))
    case _ => None
  }

  def vAddrDeps(c: Configuration) : List[Addr] = {
    val (exp, env, kaddr) = c
    exp match {
      case LetForm(v, value, body) if isAtomic(value) => {
        addressOf(value, env).toList
      }
      case LetForm(v, If(cond, tBranch, eBranch), body) => {
        addressOf(cond, env).toList
      }
      case LetForm(_, SetVar(v, ae), body) => {
        addressOf(ae, env).toList
      }
      case LetForm(v, AppForm(f, args), body) => {
        for (exp <- f :: args; addr <- addressOf(exp, env)) yield addr
      }
      case If(cond, tBranch, eBranch) => {
        addressOf(cond, env).toList
      }
      case SetVar(v, ae) => {
        addressOf(ae, env).toList
      }
      case AppForm(f, args) => {
        for (exp <- f :: args; addr <- addressOf(exp, env)) yield addr
      }
      case ae if isAtomic(ae) => {
        addressOf(ae, env).toList
      }
    }
  }

  def kAddrDeps(c: Configuration): List[KAddr] = c match {
    case (_, _, k) => List(k)
    //todo: we only should return the address if it is a return state
  }

  type Configuration = (Exp, Env, KAddr)

  def unwiden(c: Conf): Option[(Configuration, Store, KStore)] = c match {
    case (PState(exp, env, store, kaddr), kstore) => Some((exp, env, kaddr), store, kstore)
    case _ => None
  }

  def widen(c: Configuration, s: Store, k: KStore): Conf = {
    val (exp, env, kaddr) = c
    (PState(exp, env, new SentinelStore[Addr, Val](s), kaddr), new SentinelStore[KAddr, AKont](k))
  }

  def widen(c: Conf, s: Store, k: KStore): Conf = c match {
    case (PState(exp, env, _, kptr), _) => (PState(exp, env, s, kptr), k)
    case (state, _) => (state, k)
  }

  /**
   * Kleene iteration of a work set of states
   */
  private def iterateKCFA(initialState: Conf): (Set[(Conf, Conf)], Set[Conf]) = {
    var edges = Set[Edge]()
    var count = 0

    var Some((init, globalVStore, globalKStore)) = unwiden(initialState)

    val vdeps = scala.collection.mutable.HashMap[Addr, Set[Configuration]]()
    val kdeps = scala.collection.mutable.HashMap[KAddr, Set[Configuration]]()

    val seen = scala.collection.mutable.Set[Configuration]()
    var todo = List[Configuration](init)

    seen.add(init)
    for (a <- vAddrDeps(init)) vdeps(a) = vdeps.getOrElse(a, Set()) + init
    for (a <- kAddrDeps(init)) kdeps(a) = kdeps.getOrElse(a, Set()) + init

    while (todo.nonEmpty) {
      val current = todo.head
      todo = todo.tail

      val conf = widen(current, globalVStore, globalKStore)
      val nexts = mnext(conf)
      val newEdges = nexts.map(s => (conf, s))

      edges ++= newEdges
      count += 1

      println(progressPrefix + " " + count + " states computed so far.")

      for (n <- nexts; (next, nextVStore, nextKStore) <- unwiden(n)) {

        if (!seen.contains(next)) {
          seen.add(next)
          todo = next :: todo
          for (a <- vAddrDeps(next)) vdeps(a) = vdeps.getOrElse(a, Set()) + next
          for (a <- kAddrDeps(next)) kdeps(a) = kdeps.getOrElse(a, Set()) + next
        }

        val vdelta = nextVStore match {
          case s: LoggingStore[Addr, Val] => s.changeLog
        }
        if (!vdelta.isEmpty) {
          globalVStore = vdelta(globalVStore)

          for {
            a <- vdelta.dependencies()
            dep <- vdeps.getOrElse(a, Set())
          } todo = dep :: todo
        }

        val kdelta = nextKStore match {
          case s: LoggingStore[KAddr, AKont] => s.changeLog
        }
        if (!kdelta.isEmpty) {
          globalKStore = kdelta(globalKStore)

          for {
            a <- kdelta.dependencies()
            dep <- kdeps.getOrElse(a, Set())
          } todo = dep :: todo
        }

      }
    }

    println("States explored: " + count)

    val finalEdges = edges map {
      case (c1, c2) => (widen(c1, globalVStore, globalKStore), widen(c2, globalVStore, globalKStore))
    }
    val accumStates = finalEdges.map(_._1) ++ finalEdges.map(_._2)

    (finalEdges, accumStates)
  }

  type Edge = (Conf, Conf)

  def evaluateKCFA(e: Exp): (Set[Edge], Set[Conf]) = {
    iterateKCFA(initState(e))
  }

}
