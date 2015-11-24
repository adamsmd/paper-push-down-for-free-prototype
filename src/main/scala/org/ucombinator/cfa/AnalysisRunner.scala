package org.ucombinator.cfa

import org.ucombinator.util._

/**
 * @author Ilya Sergey
 */
abstract class AnalysisRunner(opts: CFAOptions) extends FancyOutput {

  type ControlState

  def k = opts.k

  def kallocPolicy = opts.kallocPolicy

  def isDummy = opts.dummy

  def simplify = opts.simplifyGraph

  lazy val isVerbose = opts.verbose

  lazy val progressPrefix = ("[" + StringUtils.trimFileName(opts.fileName) + ", "
    + getAnalysisKind(opts) + "]")

  def shouldGC = opts.gc

  def dumpDSG = opts.dumpGraph

  def printGCDebug = opts.gcDebug

  def interrupt = opts.interrupt

  def interruptAfter = opts.interruptAfter

  /**
   * Pretty-print analysis type
   */
  def getAnalysisKind(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-CFA"
      case AnalysisType.PDCFA => "-PDCFA"
    }

    val analysis = if (opts.dummy) {
      "dummy"
    } else {
      opts.k
    }
    val withGc = if (opts.gc) "-gc" else ""
    analysis + cfa + withGc
  }

  def dumpStatistics(opts: CFAOptions, stat: CFAStatistics): String = {
    import java.io._

    val CFAStatistics(time, size, vars, singletons, states, visited, edges, interrupted) = stat

    val buffer = new StringBuffer()
    buffer.append("Expressions: " + size + "\n")
    buffer.append("Visited states: " + visited + "\n")
    buffer.append("Control states: " + states + "\n")
    buffer.append("Transitions / DSG edges: " + edges + "\n")
    buffer.append("Total amount of variables: " + vars + "\n")
    buffer.append("Singletons: " + singletons + "\n")
    buffer.append("Analysis run for: " + time + " milliseconds\n")
    if (interrupted) {
      buffer.append("Interrupted after " + opts.interruptAfter + " states.")
    }

    if (isVerbose) {
      println(buffer.toString)
    }

    if (opts.dumpStatistics) {
      val statDir = new File(statisticsDirName)
      if (!statDir.exists) {
        statDir.mkdirs()
        statDir.createNewFile()
      }

      val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.fileName)
      val subfolder = new File(subfolderPath)
      if (!subfolder.exists) {
        subfolder.mkdirs()
        subfolder.createNewFile()
      }


      val path = subfolderPath + File.separator + getStatisticsDumpFileName(opts)
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("Statistics dumped into: " + path)

      path
    } else ""
  }


}
