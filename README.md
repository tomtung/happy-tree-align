# Happy Tree Align
This is a implementation of the method described in **Burkett, D., & Klein, D. (2012) Transforming trees to improve syntactic convergence**. For binlingual corpora, it learns syntax tree transformation rules to improve their agreement with corresponding word alignments. This improves the performance of syntactic machine translation systems.

## Usage

The program can be built with [sbt](http://www.scala-sbt.org/).

`sbt one-jar` will produce a stand-alone jar package callable form command line.

```
Usage: happy-tree-align learn [options] <tree.train> <align.train> <tree.dev> <align.dev>

  -n <value> | --n-trans <value>
        max number of transformations to learn (default 200)
  -o <value> | --out <value>
        output path (print to stdout by defult)
  <tree.train>
        path to training tree file
  <align.train>
        path to training alignment file
  <tree.dev>
        optional, path to dev tree file
  <align.dev>
        optional, path to dev alignment file


Usage: happy-tree-align apply [options] <trans> <in-tree> <out-tree>

  -n <value> | --n-trans <value>
        use first n transformations (use all by default)
  <trans>
        transformation sequence file
  <in-tree>
        input tree file
  <out-tree>
        output tree file
```

## Peformance

In our experiment, using a 3000-sentence training data set, and a 1000-sentence development data set, on a duo core 2.53GHz machine, it takes 16 minutes to learn the first 100 rules, and 70 minutes to learn all 3979 rules.

The implementation is parallelized, so it should take less time with more cores available.