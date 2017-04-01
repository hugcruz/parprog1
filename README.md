# parprog1

Personal repo for "Parallel Programming" course on coursera

* week1: Parallel Programming (Parallel Box Blur Filter)
* week2: Basic Task Parallel Algorithms (Reductions and Prefix Sums)
* week3: Data-Parallelism (K-Means)
* week4: Data Structures for Parallel Computing (Barnes-Hut Simulation)

## Getting Started

Install pre-requisites, clone the project, go to each week's folder and use sbt to run application and tests.

```
sbt run
```

```
sbt test
```

See next sections for details

### Prerequisites

1. [Java Development Kit](http://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html)
2. [Scala Build Tool](http://www.scala-sbt.org/release/docs/Setup.html)
3. A terminal/shell/command line (or a Scala IDE)

### Running

Using a terminal, go to the root folder of each week.

Use sbt to run.

```
sbt run
```

Note: 
Each week's project has multiple entry points/"main methods", so these will be listed when running with sbt. Just press the number relative to the "main" you want to execute and press enter.

#### Week 1: Parallel Programming (Parallel Box Blur Filter)
1. scalashop.HorizontalBoxBlurRunner - Scalameter tests
2. scalashop.ScalaShop - Main GUI
3. scalashop.VerticalBoxBlurRunner - Scalameter tests
#### Week 2: Basic Task Parallel Algorithms (Reductions and Prefix Sums)
1. reductions.LineOfSightRunner - Scalameter tests
2. reductions.ParallelCountChangeRunner - Scalameter tests
3. reductions.ParallelParenthesesBalancingRunner - Scalameter tests
#### Week 3: Data-Parallelism (K-Means)
1. kmeans.KMeansRunner - Scalameter tests
2. kmeans.fun.ScalaShop - Main GUI
#### Week 4: Data Structures for Parallel Computing (Barnes-Hut Simulation)
1. barneshut.BarnesHut - Main GUI
2. barneshut.conctrees.ConcBufferRunner - Scalameter tests

## Running the tests

Using a terminal, go to the root folder of each week.

Use sbt to run the tests.
```
sbt test
```

## Debugging

If you want to step through the code, I recomend installing the [Scala IDE](http://scala-ide.org/).

Follow the instructions on the website on how to [import](http://scala-ide.org/docs/current-user-doc/gettingstarted/index.html#Import_an_SBT_project) and [debug](http://scala-ide.org/docs/current-user-doc/features/scaladebugger/index.html#using-the-scala-debugger) a project.
