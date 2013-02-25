package edu.isi.nlg.happytreealign

import com.typesafe.scalalogging.slf4j.Logging
import java.io.{PrintWriter, File}
import java.util
import collection.parallel.immutable.ParVector
import util.concurrent.atomic.AtomicLongArray
import annotation.tailrec
import collection.JavaConversions._

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty || args(0) != "learn" && args(0) != "apply") {
      MainLearn.learnConfigParser.showUsage
      MainApply.applyConfigParser.showUsage
      sys.exit(1)
    }

    args(0) match {
      case "learn" =>
        MainLearn(args.drop(1))
      case "apply" =>
        MainApply(args.drop(1))
    }
  }
}

object MainLearn extends Logging {

  val defaultNIter = 200

  case class LearnConfig(trainTreePath: String,
                         trainAlignPath: String,
                         devTreePathOp: Option[String] = None,
                         devAlignPathOp: Option[String] = None,
                         nIter: Int = defaultNIter,
                         outPathOp: Option[String] = None)

  val learnConfigParser = new scopt.immutable.OptionParser[LearnConfig]("happy-tree-align learn") {
    override def options = Seq(
      arg("<tree.train>", "path to training tree file") {
        (s, c) => c.copy(trainTreePath = s)
      },
      arg("<align.train>", "path to training alignment file") {
        (s, c) => c.copy(trainAlignPath = s)
      },
      argOpt("<tree.dev>", "optional, path to dev tree file") {
        (s, c) => c.copy(devTreePathOp = Some(s))
      },
      argOpt("<align.dev>", "optional, path to dev alignment file") {
        (s, c) => c.copy(devAlignPathOp = Some(s))
      },
      intOpt("n", "n-trans", s"max number of transformations to learn (default $defaultNIter)") {
        (i, c) => c.copy(nIter = i)
      },
      opt("o", "out", "output path (print to stdout by defult)") {
        (s, c) => c.copy(outPathOp = Some(s))
      }
    )
  }

  def apply(args: Seq[String]) {
    val config = learnConfigParser.parse(args, LearnConfig(null, null)).getOrElse {
      sys.exit(1)
    }

    def logScores(nIter: Int,
                  transformation: Transformation,
                  trainAlignTrees: ParVector[AlignmentTree],
                  devAlignTreesOp: Option[ParVector[AlignmentTree]]) {
      val trainScore = trainAlignTrees.map(_.agreementScore).sum
      devAlignTreesOp match {
        case None =>
          logger.info(s"Iteration $nIter:\t$transformation\tTrain Score: $trainScore")
        case Some(devAlignTrees) =>
          val devScore = devAlignTreesOp.get.map(_.agreementScore).sum
          logger.info(s"Iteration $nIter:\t$transformation\tTrain Score: $trainScore\tDev Score: $devScore")
      }
    }

    val bestTransformations = new util.ArrayList[Transformation](config.nIter)
    @tailrec
    def findBestTransformations(trainAlignTrees: ParVector[AlignmentTree],
                                devAlignTreesOp: Option[ParVector[AlignmentTree]],
                                currIter: Int) {
      lazy val transOp = TransformationExtractor.findBestTransformation(trainAlignTrees)
      if (currIter <= config.nIter && transOp.isDefined) {
        val trans = transOp.get
        bestTransformations.add(trans)

        val transformedTrainAlignTrees = trainAlignTrees.map(trans(_))
        val transformedDevAlignTreesOp = for (devAlignTrees <- devAlignTreesOp) yield devAlignTrees.map(trans(_))

        logScores(
          currIter,
          trans,
          transformedTrainAlignTrees,
          transformedDevAlignTreesOp)

        for (outPath <- config.outPathOp) {
          if (currIter % 20 == 0) {
            logger.info("Saving transformation sequence up till now...")
            TransformationsSerializer.serialize(bestTransformations, new File(outPath))
          }
        }

        findBestTransformations(
          transformedTrainAlignTrees,
          transformedDevAlignTreesOp,
          currIter + 1)
      }
    }


    val trainAlignTrees: ParVector[AlignmentTree] =
      AlignmentTree.parseAlignmentTrees(config.trainTreePath, config.trainAlignPath).toVector.par

    val devAlignTreesOp: Option[ParVector[AlignmentTree]] =
      (config.devTreePathOp, config.devAlignPathOp) match {
        case (Some(devTreePath), Some(devAlignPath)) =>
          Some(AlignmentTree.parseAlignmentTrees(devTreePath, devAlignPath).toVector.par)
        case (None, None) =>
          None
        case _ =>
          learnConfigParser.showUsage
          sys.exit(1)
      }
    logScores(0, null, trainAlignTrees, devAlignTreesOp)
    logger.trace("Start learning...")
    findBestTransformations(trainAlignTrees, devAlignTreesOp, currIter = 1)

    config.outPathOp match {
      case Some(path) =>
        TransformationsSerializer.serialize(bestTransformations, new File(path))
      case None =>
        TransformationsSerializer.serialize(bestTransformations, System.out)
    }
  }

}

object MainApply extends Logging {

  case class ApplyConfig(transPath: String,
                         inTreePath: String,
                         outTreePath: String,
                         nTransOp: Option[Int] = None,
                         alignPathOp: Option[String] = None)

  val applyConfigParser = new scopt.immutable.OptionParser[ApplyConfig]("happy-tree-align apply") {

    override def options = Seq(
      arg("<trans>", "transformation sequence file") {
        (s, c) => c.copy(transPath = s)
      },
      arg("<in-tree>", "input tree file") {
        (s, c) => c.copy(inTreePath = s)
      },
      arg("<out-tree>", "output tree file") {
        (s, c) => c.copy(outTreePath = s)
      },
      intOpt("n", "n-trans", "use first n transformations (use all by default)") {
        (i, c) => c.copy(nTransOp = Some(i))
      },
      opt("align", "path to alignment file; agreement scores after each transformation will be reported if provided") {
        (s, c) => c.copy(alignPathOp = Some(s))
      }
    )
  }

  def apply(args: Seq[String]) {
    val config = applyConfigParser.parse(args, ApplyConfig(null, null, null)).getOrElse {
      sys.exit(1)
    }

    val transformations = {
      val array = TransformationsSerializer.deserialize(new File(config.transPath))
      config.nTransOp match {
        case Some(nTrans) =>
          array.toVector.take(nTrans)
        case None =>
          array.toVector
      }
    }

    // scores(i) = total score after i transformations
    val scores = new AtomicLongArray(transformations.length + 1)
    // nNonTerms(i) = total number of non-terminal symbols after i transformations
    val nNonTerms = new AtomicLongArray(transformations.length + 1)

    val writer = new PrintWriter(config.outTreePath, "UTF-8")

    val chunkSize = 20000

    @tailrec
    def transformTrees(treeLines: Stream[String], alignLinesOp: Option[Stream[String]], counter: Int = 0) {
      if (treeLines.isEmpty) {
        logger.info("All trees transformed.")

        if (alignLinesOp.isDefined && !alignLinesOp.get.dropWhile(_.isEmpty).isEmpty) {
          logger.warn("Alignment file does not match tree file.")
        }
      } else {
        val treeLineChunk = treeLines.take(chunkSize).toVector.par
        val updatedCounter = counter + treeLineChunk.length
        val alignLineChunkOp = for (alignLines <- alignLinesOp) yield alignLines.take(chunkSize).toVector.par

        val transformedTreeLineChunk = alignLineChunkOp match {
          case None =>
            treeLineChunk.map(line =>
              if (line.isEmpty) ""
              else {
                val initTree = SyntaxTree.parse(line)
                transformations.foldLeft(initTree)((currTree, trans) => trans(currTree)).toString
              }
            )

          case Some(alignLineChunk) =>
            if (treeLineChunk.length != alignLineChunk.length) {
              logger.warn("Alignment file does not match tree file.")
            }

            (treeLineChunk zip alignLineChunk).map({
              case (treeLine, "") =>
                logger.warn("This line in tree file has no corresponding alignment: " + treeLine)
                val initTree = SyntaxTree.parse(treeLine)
                transformations.foldLeft(initTree)((currTree, trans) => trans(currTree)).toString
              case (treeLine, alignLine) =>
                val initTree = {
                  val syntaxTree = SyntaxTree.parse(treeLine)
                  val align = WordPosAlignment.parseFEPairs(alignLine)
                  new AlignmentTree(syntaxTree, align)
                }

                scores.addAndGet(0, initTree.agreementScore)
                nNonTerms.addAndGet(0, initTree.syntaxTree.nNonTerminal)

                transformations.zipWithIndex.foldLeft(initTree)(
                  (currTree, transWithIdx) => {
                    val (trans, idx) = transWithIdx
                    val transformedTree = trans(currTree)
                    scores.addAndGet(idx + 1, transformedTree.agreementScore)
                    nNonTerms.addAndGet(idx + 1, transformedTree.syntaxTree.nNonTerminal)

                    transformedTree
                  }).syntaxTree.toString
            })
        }

        transformedTreeLineChunk.seq.foreach(writer.println _)

        logger.info(s"$updatedCounter lines processed.")

        transformTrees(
          treeLines.drop(chunkSize),
          alignLinesOp.map(_.drop(chunkSize)),
          updatedCounter)
      }
    }


    val treeLines = io.Source.fromFile(config.inTreePath, "UTF-8").getLines().map(_.trim).toStream
    val alignLinesOp =
      for (alignPath <- config.alignPathOp)
      yield io.Source.fromFile(alignPath, "UTF-8").getLines().map(_.trim).toStream
    transformTrees(treeLines, alignLinesOp)
    writer.close()

    if (config.alignPathOp.isDefined) {
      System.err.println("#Transformation\tAgreement-Score\t#Non-Terminals")
      for (i <- 0 until scores.length) {
        System.err.println(i + "\t" + scores.get(i) + "\t" + nNonTerms.get(i))
      }
    }
  }

}
