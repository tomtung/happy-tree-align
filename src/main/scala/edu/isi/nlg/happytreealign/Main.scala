package edu.isi.nlg.happytreealign

import com.typesafe.scalalogging.slf4j.Logging
import java.io.{PrintWriter, File}
import java.util
import collection.parallel.immutable.ParVector
import util.concurrent.atomic.{AtomicLong, AtomicLongArray}
import annotation.tailrec
import collection.JavaConversions._
import scopt.immutable.OptionParser

object Main {

  val appName = "happy-tree-align"

  lazy val entrances = List(MainLearn, MainApply, MainScore).map(e => e.name -> e).toMap

  def main(args: Array[String]) {

    if (args.isEmpty || !entrances.contains(args(0))) {
      entrances.values.foreach(_.printUsage())
      sys.exit(1)
    }

    entrances(args(0))(args.drop(1))
  }
}

// This trait is bizarre in a few ways, but I don't yet have motivation to further refactor it...
trait MainEntrance {
  def name: String

  def fullName = s"${Main.appName} $name"

  type Config

  def configParser: OptionParser[Config]

  def printUsage() {
    configParser.showUsage
  }

  def apply(args: Seq[String])

}

object MainLearn extends MainEntrance with Logging {

  override val name = "learn"

  val defaultNIter = 200

  case class Config(trainTreePath: String = null,
                    trainAlignPath: String = null,
                    devTreePathOp: Option[String] = None,
                    devAlignPathOp: Option[String] = None,
                    nIter: Int = defaultNIter,
                    outPathOp: Option[String] = None)

  override val configParser = new OptionParser[Config](this.fullName) {
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
      opt("o", "out", "output path (print to stdout by default)") {
        (s, c) => c.copy(outPathOp = Some(s))
      }
    )
  }

  override def apply(args: Seq[String]) {
    val config = configParser.parse(args, Config()).getOrElse {
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
          configParser.showUsage
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

object MainApply extends MainEntrance with Logging {

  override val name = "apply"

  case class Config(transPath: String = null,
                    inTreePath: String = null,
                    outTreePath: Option[String] = None,
                    nTransOp: Option[Int] = None,
                    alignPathOp: Option[String] = None)

  override val configParser = new OptionParser[Config](this.fullName) {

    override def options = Seq(
      arg("<trans>", "transformation sequence file") {
        (s, c) => c.copy(transPath = s)
      },
      arg("<in-tree>", "input tree file") {
        (s, c) => c.copy(inTreePath = s)
      },
      argOpt("<align>", "optional, path to alignment file. if provided, report agreement scores after each transformation") {
        (s, c) => c.copy(alignPathOp = Some(s))
      },
      opt("o", "out", "output path. print to stdout by default") {
        (s, c) => c.copy(outTreePath = Some(s))
      },
      intOpt("n", "n-trans", "use first n transformations (use all by default)") {
        (i, c) => c.copy(nTransOp = Some(i))
      }
    )
  }

  override def apply(args: Seq[String]) {
    val config = configParser.parse(args, Config()).getOrElse {
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

    def transformPrintTrees[T](transformTree: T => SyntaxTree)
                              (treeOpStream: => Stream[Option[T]]) {
      val chunkSize = 20000

      val writerOp = config.outTreePath.map(new PrintWriter(_, "UTF-8"))
      def printLine(arg: Any) {
        writerOp match {
          case Some(writer) =>
            writer.println(arg)
          case None =>
            println(arg)
        }
      }

      @tailrec
      def doTransformAndPrint(treeOpStream: Stream[Option[T]], counter: Int = 0) {
        if (treeOpStream.isEmpty) {
          logger.info("All trees transformed")
        } else {
          val (chunk, rest) = treeOpStream.splitAt(chunkSize)

          chunk.toVector.par.map {
            case Some(tree) =>
              transformTree(tree).toString
            case None => ""
          }.seq.foreach(printLine)

          val updatedCounter = counter + chunk.length
          logger.info(s"$updatedCounter lines processed.")

          doTransformAndPrint(rest, updatedCounter)
        }
      }

      doTransformAndPrint(treeOpStream)
      writerOp.foreach(_.close())
    }

    config.alignPathOp match {
      case Some(alignPath) =>
        def aTreeOpStream = AlignmentTree.parseAlignmentTreesOp(config.inTreePath, alignPath).toStream
        // scores(i) = total score after i transformations
        val scores = new AtomicLongArray(transformations.length + 1)
        // nNonTerms(i) = total number of non-terminal symbols after i transformations
        val nNonTerms = new AtomicLongArray(transformations.length + 1)

        def transformATree(originTree: AlignmentTree): SyntaxTree = {

          scores.addAndGet(0, originTree.agreementScore)
          nNonTerms.addAndGet(0, originTree.syntaxTree.nNonTerminal)

          transformations.zipWithIndex.foldLeft(originTree)(
            (currTree, transWithIndex) => {
              val (trans, index) = transWithIndex
              val transformedTree = trans(currTree)

              scores.addAndGet(index + 1, transformedTree.agreementScore)
              nNonTerms.addAndGet(index + 1, transformedTree.syntaxTree.nNonTerminal)

              transformedTree
            }
          ).syntaxTree
        }

        transformPrintTrees(transformATree)(aTreeOpStream)

        System.err.println("#Transformation\tAgreement-Score\t#Non-Terminals")
        for (i <- 0 until scores.length) {
          System.err.println(i + "\t" + scores.get(i) + "\t" + nNonTerms.get(i))
        }

      case None =>
        def sTreeOpStream = io.Source.fromFile(config.inTreePath, "UTF-8").getLines().map(_.trim).map(line => {
          if (line.isEmpty) None
          else Some(SyntaxTree.parse(line))
        }).toStream

        def transformSTree(originTree: SyntaxTree): SyntaxTree =
          transformations.foldLeft(originTree)(
            (currTree, trans) => trans(currTree)
          )

        transformPrintTrees(transformSTree)(sTreeOpStream)
    }

  }

}

object MainScore extends MainEntrance with Logging {

  override val name = "score"

  case class Config(treePath: String = null,
                    alignPath: String = null,
                    outputAll: Boolean = false,
                    outputPathOp: Option[String] = None)

  val configParser = new OptionParser[Config](this.fullName) {
    override def options = Seq(
      arg("<tree>", "tree file") {
        (s, c) => c.copy(treePath = s)
      },
      arg("<align>", "alignment file") {
        (s, c) => c.copy(alignPath = s)
      },
      flag("a", "all", "output scores for all trees") {
        c => c.copy(outputAll = true)
      },
      opt("o", "out", "output path. print to stdout by default") {
        (s, c) => c.copy(outputPathOp = Some(s))
      }
    )
  }

  override def apply(args: Seq[String]) {
    val config = configParser.parse(args, Config()).getOrElse {
      sys.exit(1)
    }

    val writerOp = config.outputPathOp.map(new PrintWriter(_, "UTF-8"))

    def printLine[T](arg: T) {
      writerOp match {
        case Some(writer) =>
          writer.println(arg)
        case None =>
          println(arg)
      }
    }

    val chunkSize = 20000

    val totalScore = new AtomicLong(0)
    val totalNNonTerminal = new AtomicLong(0)

    @tailrec
    def scoreTrees(treeStream: Stream[AlignmentTree], counter: Int = 0) {
      if (!treeStream.isEmpty) {
        val vec = treeStream.take(chunkSize).toVector.par.map(t => {
          totalScore.addAndGet(t.agreementScore)
          totalNNonTerminal.addAndGet(t.syntaxTree.nNonTerminal)
          (t.agreementScore, t.syntaxTree.nNonTerminal)
        })
        if (config.outputAll) {
          vec.seq.foreach({
            case (score, nNonTerm) =>
              printLine(s"$score\t$nNonTerm")
          })
        }

        val updatedCounter = counter + vec.length
        logger.info(s"$updatedCounter trees processed.")

        scoreTrees(treeStream.drop(chunkSize), updatedCounter)
      }
    }

    def alignTreesStream = AlignmentTree.parseAlignmentTrees(config.treePath, config.alignPath).toStream
    if (config.outputAll) {
      printLine("Score\t#Non-terminal")
    }
    scoreTrees(alignTreesStream)
    printLine(s"Total Score: ${totalScore.get()}")
    printLine(s"Total #Non-terminal: ${totalNNonTerminal.get()}")

    writerOp.foreach(_.close())
  }
}
