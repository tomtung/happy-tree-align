# Happy Tree Align [![Build Status](https://api.travis-ci.org/tomtung/happy-tree-align.png)](https://travis-ci.org/tomtung/happy-tree-align)

This is a implementation of the method described in [Burkett, D., & Klein, D. (2012) Transforming trees to improve syntactic convergence](http://www.cs.berkeley.edu/~dburkett/papers/burkett12-tree_transform.pdf). For binlingual corpora, it learns syntax tree transformation rules to improve their agreement with corresponding word alignments. This improves the performance of syntactic machine translation systems.

The name comes from the author's keynote [Happy Trees are Better than Correct Trees](http://www.cs.berkeley.edu/~dburkett/slides/burkett12-tree_transform-slides.pdf).

## Usage

The program can be built with [sbt](http://www.scala-sbt.org/).

`sbt one-jar` will produce a stand-alone jar package callable form command line.

```
Usage: happy-tree-align learn [options] <tree.train> <align.train> <tree.dev> <align.dev>

  -n <value> | --n-trans <value>
        max number of transformations to learn (default 200)
  -o <value> | --out <value>
        output path (print to stdout by default)
  <tree.train>
        path to training tree file
  <align.train>
        path to training alignment file
  <tree.dev>
        optional, path to dev tree file
  <align.dev>
        optional, path to dev alignment file


Usage: happy-tree-align apply [options] <trans> <in-tree> <align>

  -o <value> | --out <value>
        output path. print to stdout by default
  -n <value> | --n-trans <value>
        use first n transformations (use all by default)
  <trans>
        transformation sequence file
  <in-tree>
        input tree file
  <align>
        optional, path to alignment file. if provided, report agreement scores after each transformation


Usage: happy-tree-align score [options] <tree> <align>

  -a | --all
        output scores for all trees
  -o <value> | --out <value>
        output path. print to stdout by default
  <tree>
        tree file
  <align>
        alignment file
```

## Peformance

In our experiment, using a 3000-sentence training data set, and a 1000-sentence development data set, on a duo core 2.53GHz machine, it takes 16 minutes to learn the first 100 rules, and 70 minutes to learn all 3979 rules.

The implementation is parallelized, so it should take less time with more cores available.
