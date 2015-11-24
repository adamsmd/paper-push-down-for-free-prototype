This is a prototype implementation comparing continuation allocation strategies in an abstract interpreter. It is based on the project:

https://github.com/ilyasergey/reachability.

### Dependencies ###

* sbt 0.13
* Scala 2.11

### Building ###

To compile the project run `sbt compile` which places the class files in the target directory.

The main class is org.ucombinator.cfa.RunCFA.

### Note ###

The number of states visited may vary from run to run for some benchmarks. This is because the order in which states are visited is partially determined by the iteration order of sets in Scala which is not consistent from run to run. The order in which states are visited determines how the analysis moves up the lattice. Some state transitions jump further up the lattice than others. Moving up the lattice more quickly results in fewer states being visited.
