package org.ucombinator.scheme.benchmarks.pdcfa

import org.ucombinator.cfa.RunCFA

/**
 * @author Ilya Sergey
 */
trait PDCFABenchmarkParams {

  val args_0_KCFA_GC = Array("--kcfa", "--k", "0", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_KCFA = Array("--kcfa", "--k", "0",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_KCFA_GC = Array("--kcfa", "--k", "1", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_KCFA = Array("--kcfa", "--k", "1",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_PDCFA_GC = Array("--pdcfa", "--k", "0", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_PDCFA = Array("--pdcfa", "--k", "0",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_PDCFA_GC = Array("--pdcfa", "--k", "1", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_PDCFA = Array("--pdcfa", "--k", "1",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  def tryAndRecover(prefixMessage: String, filePath: String, run: () => Unit) {
    println()
    println(prefixMessage + filePath)
    println()

    try {
      run()
    } catch {
      case e: Exception => {
        println(prefixMessage + "failed with the exception:")
        println(e.getMessage)
        System.err.println(e.getStackTraceString)
      }
    }
  }

  def run0CFAs(filePath: String) {

    tryAndRecover("Running 0-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_PDCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 0-KCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_KCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 0-PDCFA for ", filePath,
      (() => RunCFA.main(args_0_PDCFA ++ Array(filePath))))

    tryAndRecover("Running 0-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_KCFA ++ Array(filePath))))

  }

  def run1CFAs(filePath: String) {

    tryAndRecover("Running 1-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_PDCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 1-KCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_KCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 1-PDCFA for ", filePath,
      (() => RunCFA.main(args_1_PDCFA ++ Array(filePath))))

    tryAndRecover("Running 1-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_KCFA ++ Array(filePath))))

  }

  def runForAll(filePath: String) {
    println()
    println("Running benchmars suite for " + filePath)
    println()

    run0CFAs(filePath)
    run1CFAs(filePath)

    println()
    println("Suite is successfully finished for " + filePath)
    println()

  }

}
