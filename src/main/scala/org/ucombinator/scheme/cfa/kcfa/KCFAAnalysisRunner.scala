package org.ucombinator.scheme.cfa.kcfa

import org.ucombinator.scheme.syntax._
import org.ucombinator.util._
import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.scheme.cfa.SchemeCFARunner
import org.ucombinator.cfa.CFAStatistics

/**
 * @author Ilya Sergey
 */
class KCFAAnalysisRunner(opts: CFAOptions) extends SchemeCFARunner(opts) with PointerCESKMachinery with KCFAGarbageCollector
with FancyOutput {

  def runKCFA(opts: CFAOptions, anast: Exp) {
    val sizeExp = ANormalizer.size(anast)


    val firstTime = (new java.util.Date()).getTime

    val (resultEdges, resultConfs, stateCount): (Set[(Conf, Conf)], Set[Conf], Int) =
      evaluateKCFA(anast)

    val secondTime = (new java.util.Date()).getTime
    val delta = (secondTime - firstTime)

    if (isVerbose) {
      println()
      println("The analysis has taken " + (
        if (delta / 1000 < 1) "less than one second."
        else if (delta / 1000 == 1) "1 second."
        else delta / 1000 + " seconds."))
    }

    if (opts.verbose) {
      println()
      println("Finished. Computed states: " + resultConfs.size)
    }

    if (!opts.simplifyGraph &&
      resultConfs.exists {
        case (PFinal(_), _) => true
        case _ => false
      }) {
      if (opts.verbose) {
        println("Has final state.\n")
      }
    } else if (!opts.simplifyGraph) {
      println("Warning: no final state!\n")
    }

    if (isVerbose) {
      println()
      println("Computing statistics")
      println()
    }
    val controlStates: Set[ControlState] = resultConfs.map(_._1)

    var stateCounter = 0
    val map: Map[ControlState, Int] = controlStates.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap.asInstanceOf[Map[ControlState, Int]]


    val intNodes: Set[Int] = map.values.toSet
    val intEdges: Set[(Int, Int)] = resultEdges.flatMap[(Int, Int), Set[(Int, Int)]]{
      case (c, c1) => if (map.isDefinedAt(c._1) && map.isDefinedAt(c1._1)){
        Set((map.apply(c._1), map.apply(c1._1)))
      } else Set.empty
    }


    val (allVars, singletons) = computeSingletons(controlStates, anast)
    val interrupted = opts.interrupt && resultConfs.size > opts.interruptAfter

    dumpStatistics(opts, CFAStatistics(delta, sizeExp, allVars.size,
      singletons.size, intNodes.size, stateCount, intEdges.size, interrupted))

    if (interrupt) {
      println ("Interrupted after " + resultConfs.size + " states visited")
    }


    if (opts.dumpGraph) {
      println()
      println("Writing State Transition Graph")
      println()
      val path = dumpTransitionGraph(opts, resultConfs, resultEdges)
      println("Transition Graph dumped into " + path)
    }
  }

  /**
   * Prints transition graph according to the passed parameters
   */
  def prettyPrintTransitions(states: Set[Conf], edges: Set[(Conf, Conf)]): String = {

    val controlStates: Set[ControlState] = states.map(x => x._1)
    var stateCounter = 0
    val map: Map[ControlState, Int] = controlStates.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap.asInstanceOf[Map[ControlState, Int]]

    println("Control states: " + controlStates.size)

    val buffer = new StringBuffer
    buffer.append("digraph BST {\nsize=\"6,4\" ; ratio = fill;\n ")

    var list: List[String] = List()
    for (edge <- edges: Set[(Conf, Conf)]) {
      val buf = new StringBuffer()
      val (s, _) = edge._1
      val (s1, _) = edge._2

      buf.append("\"" + prettyPrintState(s, map) + "\"")
      buf.append(" -> ")
      buf.append("\"" + prettyPrintState(s1, map) + "\"")

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }

  def dumpTransitionGraph(opts: CFAOptions, states: Set[Conf], edges: Set[(Conf, Conf)]): String = {
    import java.io._

    val graphs = new File(graphsDirName)
    if (!graphs.exists) {
      graphs.mkdirs()
      graphs.createNewFile()
    }

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(opts.fileName)
    val subfolder = new File(subfolderPath)
    if (!subfolder.exists) {
      subfolder.mkdirs()
      subfolder.createNewFile()
    }


    val path = subfolderPath + File.separator + getGraphDumpFileName(opts)
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
    writer.write(prettyPrintTransitions(states, edges))
    writer.close()
    path
  }


}
