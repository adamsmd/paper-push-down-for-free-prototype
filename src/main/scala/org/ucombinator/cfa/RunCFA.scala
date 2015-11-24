package org.ucombinator.cfa

import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.scheme.parsing.RnRSParser
import org.ucombinator.util._
import org.ucombinator.scheme.cfa.kcfa.KCFAAnalysisRunner
import org.ucombinator.scheme.cfa.pdcfa.PDCFAAnalysisRunner
import AnalysisType._

/**
 * @author Ilya Sergey
 */
object RunCFA {

  val version: String = "20120619"
  val versionString: String = "    Version " + version + "\n"

  val helpMessage = (" GenericCFA - a runner for k-CFA and Push-down k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar GenericCFA.jar [--lang lang][--pdcfa | --kcfa] [--k k] [--gc] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where

    --pdcfa                run Pushdown k-CFA (run by default)
    --kcfa                 run classic k-CFA
    --k k                  "k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --kalloc policy        continuation address allocation policy (p4f,aac,kcfa)
    --gc                   switch on abstract Garbage Collector (default = off)
    --dump-graph           dump Transition/Dyck State Graphs into a GraphViz file ./graphs/filename/graph-(analysis-type).gv
    --dump-statistics      dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    interrupts the analysis after n states computed (default = off)
    --help                 print this message
    --verbose              print additional information on the analysis and results
    filePath               path to a file to be analysed
    """)


  def main(args: Array[String]) {

    val opts = CFAOptions.parse(args)

    if (args.size == 0 || opts.help) {
      println(helpMessage)
      return
    }

    if (opts.fileName == null) {
      println()
      System.err.println("Please, specify a filename to process")
      println()
      println(helpMessage)
      return
    }

    import org.ucombinator.scheme.syntax._

    val filename = opts.fileName
    if (opts.verbose) {
      System.err.print("Parsing s-expressions...")
    }
    val sexps = SExp.parseAllIn(filename)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.err.print("Bulding AST...")
    }
    val ast = RnRSParser(sexps)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.err.print("A-normalizing...")
    }
    val anast: Exp = ANormalizer(ast)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.out.println("Input program:")
      System.out.println(ast)
      System.out.println("\n")

      System.out.println("ANF program:")
      System.out.println(anast)
      System.out.println("\n")
    }

    opts.analysisType match {
      case PDCFA => {
        val runner = new PDCFAAnalysisRunner(opts)
        runner.runPDCFA(opts, anast)
      }
      case KCFA => {
        val runner = new KCFAAnalysisRunner(opts)
        runner.runKCFA(opts, anast)
      }
    }
  }

}


