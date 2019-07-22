# Multiplex Page Rank
## Introduction
This Multiplex Page Rank is an adaptation of the [Matlab version made by Ginestra Bianconi](https://github.com/ginestrab/Multiplex-PageRank). It is written in Scala with [Breeze](https://github.com/scalanlp/breeze).

## Installation
Requirements: Scala 2.13, Breeze 1.0-RC4, sbt 1.0

```
sbt clean compile package
```

Then create a lib folder in your project, and put the jar in it.

## Usage Instruction

To import the Multiplex Page Rank, use:

```
import pagerank_multiplex.core._
```

You can find example of use [here](https://github.com/AnnabelleGillet/PageRank-Multiplex/blob/master/src/test/scala/pagerank_multiplex/core/PageRankMultiplexTest.scala).
