package org.ucombinator.util

object AnalysisType extends Enumeration {
  type AnalysisType = Value
  val PDCFA, KCFA = Value
}

import AnalysisType._

class CFAOptions {
  var fileName: String = null
  var help: Boolean = false
  var k: Int = 0
  var m: Int = 1
  var kallocPolicy = "p4f"
  var printStates = false
  var flatPolicy = "m"
  var analysis = "flat"
  var analysisType: AnalysisType = PDCFA
  var verbose = false
  var dumpStatistics = false
  var simplifyGraph = false
  var dummy = false
  var gc = false
  var gcDebug = false
  var dumpGraph = false

  var interrupt = false
  var interruptAfter = 0
}


object CFAOptions {

  def parse(args: List[String], opts: CFAOptions) {
    args match {
      case List() => {}
      case "--k" :: k :: rest => {
        opts.k = Integer.parseInt(k)
        parse(rest, opts)
      }

      case "--help" :: rest => {
        opts.help = true
        parse(rest, opts)
      }

      case "--dummy" :: rest => {
        opts.dummy = true
        parse(rest, opts)
      }

      case "--simple-graph" :: rest => {
        opts.simplifyGraph = true
        parse(rest, opts)
      }

      case "--dump-statistics" :: rest => {
        opts.dumpStatistics = true
        parse(rest, opts)
      }

      case "--kcfa" :: rest => {
        opts.analysisType = KCFA
        parse(rest, opts)
      }

      case "--kalloc" :: policy :: rest => {
        opts.kallocPolicy = policy
        parse(rest, opts)
      }

      case "--pdcfa" :: rest => {
        opts.analysisType = PDCFA
        parse(rest, opts)
      }

      case "--gc" :: rest => {
        opts.gc = true
        parse(rest, opts)
      }

      case "--gcDebug" :: rest => {
        opts.gcDebug = true
        parse(rest, opts)
      }

      case "--verbose" :: rest => {
        opts.verbose = true
        parse(rest, opts)
      }

      case "--m" :: m :: rest => {
        opts.m = Integer.parseInt(m)
        parse(rest, opts)
      }

      case "--flat-policy" :: s :: rest => {
        opts.flatPolicy = s
        parse(rest, opts)
      }

      case "--analysis" :: a :: rest => {
        opts.analysis = a
        parse(rest, opts)
      }

      case "--print-states" :: "true" :: rest => {
        opts.printStates = true
        parse(rest, opts)
      }

      case "--print-states" :: "false" :: rest => {
        opts.printStates = false
        parse(rest, opts)
      }

      case "--dump-graph" :: rest => {
        opts.dumpGraph = true
        parse(rest, opts)
      }

      case "--interrupt-after" :: v :: rest => {
        opts.interrupt = true
        opts.interruptAfter = Integer.parseInt(v)
        parse(rest, opts)
      }

      case fileName :: rest => {
        opts.fileName = fileName;
        parse(rest, opts)
      }

    }
  }

  def parse(args: Array[String]): CFAOptions = {
    val opts = new CFAOptions
    parse(args.toList, opts)
    opts
  }

}




