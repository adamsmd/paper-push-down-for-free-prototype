package org.ucombinator.scheme.benchmarks.pdcfa

/**
 * Benchmark suites
 *
 * @author Ilya Sergey
 */

// 1
object Eta extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/eta.scm"
    runForAll(path)
  }
}

// 2
object Midtgaard2009 extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/midtgaard-icfp09.scm"
    runForAll(path)
  }
}

// 3
object DoubleLoop extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/double-loop.scm"
    runForAll(path)
  }
}

// 4
object Diamond extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/diamond/diamond.scm"
    runForAll(path)
  }
}

// 5
object KcfaWorstCase extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/kcfa-worst-case.scm"
    runForAll(path)
  }
}

// 6
object KcfaEvenWorse extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/kcfa-even-worse.scm"
    runForAll(path)
  }
}

// 7
object Fermat extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/fermat.scm"
    runForAll(path)
  }
}

// 8
object RSA extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/rsa.scm"
    runForAll(path)
  }
}

// 9
object SatBrute extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/sat-brute.scm"
    runForAll(path)
  }
}

// 10
object RegexDerivative extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/regex-derivative.scm"
    runForAll(path)
  }
}

object PDCFABenchmarks
