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
  type KAddr = (Either[Var, Exp], List[AKont])

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
    val a0: KAddr = (Left(SName.gensym("mt")), Nil)
    val newKStore: KStore = (new MapStore[KAddr, AKont]() + (a0, Set(MT)))
    (PState(e, Map.empty, new MapStore(Map.empty), a0), newKStore)
  }

  /**
   * Allocate a new continuation address basing on the control state and store
   *
   * @param c control state
   * @param kstore kontinuation store
   * @return new continuation address
   */
  def kalloc(c: ControlState, kstore: AKont): KAddr = c match {
    // todo: Fix
    case PState(Let(Bindings(List(Binding(name, _))), _), rho, s, kptr) => k match {
      case 0 | 1 => (Left(name), Nil)
      //      case 1 => (Left(name), List(kont))
      case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
    }

    case PState(e, rho, s, kptr) => k match {
      case 0 | 1 => (Right(e), Nil)
      //      case 1 => (Right(e), List(kont))
      case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
    }
    case _ => throw new PointerCESKException("Wrong state to allocate a continuation address:\n" + c.toString)
  }

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

  def updateKStoreValues(kstore: KStore, a: KAddr, konts: Set[AKont]) = {
    val oldKonts = kstore.getOrElse(a, Set())
    kstore + (a, oldKonts ++ konts)
  }

  class PointerCESKException(s: String) extends CESKException(s)

  /** ******************************************************************
    * Main non-deterministic abstract step function
    * *******************************************************************/
  def mnext: Conf => Set[Conf] = {
    // Application of lambda or reference
    case c@(PState(App(f@(Lambda(_, _) | Ref(_)), args), rho, s, a), kstore) =>

      atomicEval(f, rho, s).flatMap[Conf, Set[Conf]] {
        // f refers to a closure
        case Clo(lam@Lambda(Formals(params, _), body), rho1) => {
          val paramNames = params.map(_.name)
          val ai = paramNames.map(alloc(_, c)) // allocate addresses
          val rho2 = updateEnv(rho1, paramNames.zip(ai)) // update function env

          val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s)) // map atomic arguments to values
          val s1 = updateStore(s, ai.zip(arg_vals))

          // In A-normal form only one expression in body
          val e = getLambdaBodyInANF(lam)
          mkSet(PState(e, rho2, s1, a), kstore)
        }
        // f refers to a stored primitive
        case p@PrimLit(prim, _) => embedValueToExp(p) match {
          case BadExp => Set.empty
          case exp => mnext((PState(App(exp, args), rho, s, a), kstore))
        }
        case _ => Set.empty
      }

    /**
     * Special hacky case because of (set!) desugaring
     */
    case c@(PState(l@Let(_, _), rho, s, kaddr), kstore)
      if (decomposeLetInANF(l)._2.isUnspecified) => {
      val (v, _, e) = decomposeLetInANF(l)
      val a = alloc(v, c)
      val rho1 = updateEnv(rho, List((v, a)))
      Set((PState(e, rho1, s, kaddr), kstore))
    }


    case (c@PState(l@Let(_, _), rho, s, a), kstore) => {
      val (v, call, e) = decomposeLetInANF(l)
      val frameToAdd = LetFrame(v, e, rho)
      for {
        k <- lookupKStore(kstore, a)
        b = kalloc(c, k)
        kstore1 = updateKStore(kstore, (b, Pointed(frameToAdd, a)))
      } yield (PState(call, rho, s, b), kstore1)
    }

    // return state
    case c@(PState(ae, rho, s, kaddr), kstore)
      if isAtomic(ae) => for {
      k <- lookupKStore(kstore, kaddr)
      values = atomicEval(ae, rho, s)
      next <- returnValue(k, values, kstore, c, s)
    } yield next


    /** ****************************************************
      * Conditional operator
      * *****************************************************/
    case (c@PState(b@If(cond, tBranch, eBranch), rho, s, a), kstore) => {
      val frameToAdd = IfFrame(tBranch, eBranch, rho)
      for {
        k <- lookupKStore(kstore, a)
        b = kalloc(c, k)
        kstore1 = updateKStore(kstore, (b, Pointed(frameToAdd, a)))
      } yield (PState(cond, rho, s, b), kstore1)
    }

    /** ****************************************************
      * Set!
      * *****************************************************/
    case c@(PState(SetVar(v, ae), rho, s, kaddr), kstore)
      // Only atomic values are assigned
      if (isAtomic(ae)) => {
      val addr = lookupEnv(rho, v)
      val eval = atomicEval(ae, rho, s)
      val s1 = updateStore(s, List((addr, eval)))
      for {
        k <- lookupKStore(kstore, kaddr)
        next <- returnValue(k, Set(), kstore, c, s1)
      } yield next
    }

    /** ****************************************************
      * Primitive applications
      * *****************************************************/
    // only atomic values or variable are supported in primops
    case c@(PState(app@App(p@Prim(primName, _), args), rho, s, a), kstore) => {
      // map atomic arguments to values (sets)
      val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s))
      for {
        results <- arg_vals.size match {
          case 0 => Set(evalPrimApp(primName, List()))
          case 1 => for {a <- arg_vals.head} yield evalPrimApp(primName, List(a))
          case 2 => for {
            a <- arg_vals.head
            b <- arg_vals.tail.head
          } yield evalPrimApp(primName, List(a, b))
          case n => {
            throw new PointerCESKException("Primitive functions of arity " + n + " are not supported:\n" + app.toString)
          }
        }
        result <- results
        state = analyseResult(result, rho, s, app, a)
      } yield (state, kstore)
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
  def returnValue(k: AKont,
                  values: Set[Val],
                  kstore: KStore,
                  c: Conf,
                  s: Store): Set[Conf] = {
    k match {

      // return to final state
      case MT => Set((PFinal(values), kstore))

      // return from let-statement
      case Pointed(LetFrame(v, e, rho1), b) => {
        val a = alloc(v, c)
        val rho2 = updateEnv(rho1, List((v, a)))
        val s1 = updateStore(s, List((a, values)))
        Set((PState(e, rho2, s1, b), kstore))
      }

      // return from if-statement
      case Pointed(IfFrame(tb, eb, rho1), b) => {
        var nexts = Set[Conf]()
        if (values.exists(_ != BoolLit(false)))
          nexts = nexts + ((PState(tb, rho1, s, b), kstore))
        if (values.contains(BoolLit(false)))
          nexts = nexts + ((PState(eb, rho1, s, b), kstore))
        nexts
      }
    }
  }

  def kaddrOf(c: ControlState): Option[KAddr] = c match {
    case PState(_, _, _, a) => Some(a)
    case _ => None
  }

  /**
   * Kleene iteration of a work set of states
   */
  private def iterateKCFA(initialState: Conf): (Set[(Conf, Conf)], Set[Conf]) = {
    var edges = Set[Edge]()
    var accumStates = Set[Conf]()

    val (init, store) = initialState
    var globalKStore: StoreInterface[KAddr, AKont] = store

    val deps = scala.collection.mutable.HashMap[KAddr, Set[ControlState]]()
    val seen = scala.collection.mutable.Set[ControlState]()
    var todo = List(init)

    seen.add(init)
    for (a <- kaddrOf(init)) deps(a) = deps.getOrElse(a, Set()) + init

    while (!todo.isEmpty) {
      val newState = todo.head
      todo = todo.tail

      seen.add(newState)

      val newConf = (newState, new SentinelStore(globalKStore))
      val succs = mnext(newConf)
      val newEdges = succs.map(s => (newConf, s))

      accumStates ++= succs
      edges ++= newEdges

      println(progressPrefix + " " + accumStates.size + " states computed so far.")

      for ((succState, succKStore) <- succs) {

        if (!seen.contains(succState)) {
          todo = succState :: todo
          for (a <- kaddrOf(succState)) deps(a) = deps.getOrElse(a, Set()) + succState
        }

        val delta = succKStore match {
          case s: LoggingStore[KAddr, AKont] => s.changeLog
        }
        if (!delta.isEmpty) {
          globalKStore = delta(globalKStore)

          for {
            a <- delta.dependencies()
            dep <- deps.getOrElse(a, Set())
          } todo = dep :: todo
        }
      }

    }

    (edges, accumStates)
  }

  type Edge = (Conf, Conf)

  def evaluateKCFA(e: Exp): (Set[Edge], Set[Conf]) = {
    iterateKCFA(initState(e))
  }

}
