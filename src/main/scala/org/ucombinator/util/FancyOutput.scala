package org.ucombinator.util

/**
 * @author Ilya Sergey
 */
trait FancyOutput {

  val graphsDirName: String = "graphs"

  val statisticsDirName: String = "statistics"

  type ControlState

  def isVerbose: Boolean

  def progressPrefix: String

  def shouldGC: Boolean

  def simplify: Boolean

  def printGCDebug: Boolean

  def interrupt: Boolean

  def dumpDSG: Boolean

  def interruptAfter: Int

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String

  /**
   * Get a fancy name dump files
   */
  def getGraphDumpFileName(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-" + opts.kallocPolicy
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "graph-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    prefix + arity + cfa + gc + ".gv"
  }

  def getStatisticsDumpFileName(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-" + opts.kallocPolicy
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "stat-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    prefix + arity + cfa + gc + ".txt"
  }



}
