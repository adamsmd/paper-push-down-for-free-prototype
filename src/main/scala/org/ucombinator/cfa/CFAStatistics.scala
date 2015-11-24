package org.ucombinator.cfa

/**
 * @author Ilya Sergey
 */
case class CFAStatistics(timeSec: Long,
                         numExp: Int,
                         numVars: Int,
                         numSingletons: Int,
                         numStates: Int,
                         numStatesVisited: Int,
                         numEdges: Int,
                         interrupted: Boolean)

