package edu.isi.nlg.happytreealign

import com.typesafe.scalalogging.slf4j.Logging
import com.thoughtworks.xstream.XStream
import java.io.{PrintWriter, File}
import java.util
import collection.JavaConversions._

object Main extends Logging {

  val defaultNIter = 200

  case class LearnConfig(trainTreePath: String,
                         trainAlignPath: String,
                         devTreePath: Option[String] = None,
                         devAlignPath: Option[String] = None,
                         nIter: Int = defaultNIter,
                         outPath: Option[String] = None)

  val learnConfigParser = new scopt.immutable.OptionParser[LearnConfig]("happy-tree-align learn") {
    override def options = Seq(
      arg("<tree.train>", "path to training tree file") {
        (s, c) => c.copy(trainTreePath = s)
      },
      arg("<align.train>", "path to training alignment file") {
        (s, c) => c.copy(trainAlignPath = s)
      },
      argOpt("<tree.dev>", "optional, path to dev tree file") {
        (s, c) => c.copy(devTreePath = Some(s))
      },
      argOpt("<align.dev>", "optional, path to dev alignment file") {
        (s, c) => c.copy(devAlignPath = Some(s))
      },
      intOpt("n", "n-trans", s"max number of transformations to learn (default $defaultNIter)") {
        (i, c) => c.copy(nIter = i)
      },
      opt("o", "out", "output path (print to stdout by defult)") {
        (s, c) => c.copy(outPath = Some(s))
      }
    )
  }

  case class ApplyConfig(transPath: String,
                         inTreePath: String,
                         outTreePath: String,
                         nTrans: Option[Int] = None)

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
        (i, c) => c.copy(nTrans = Some(i))
      }
    )
  }

  def main(args: Array[String]) {
    if (args.isEmpty || args(0) != "learn" && args(0) != "apply") {
      learnConfigParser.showUsage
      applyConfigParser.showUsage
      sys.exit(1)
    }

    args(0) match {
      case "learn" =>
        learnMain(args.drop(1))
      case "apply" =>
        applyMain(args.drop(1))
    }
  }

  def learnMain(args: Seq[String]) {
    val config = learnConfigParser.parse(args, LearnConfig(null, null)).getOrElse {
      sys.exit(1)
    }

    var trainAlignTrees = AlignmentTree.parseAlignmentTrees(config.trainTreePath, config.trainAlignPath).toVector.par
    var devAlignTreesOp = (config.devTreePath, config.devAlignPath) match {
      case (Some(devTreePath), Some(devAlignPath)) =>
        Some(AlignmentTree.parseAlignmentTrees(devTreePath, devAlignPath).toVector.par)
      case (None, None) =>
        None
      case _ =>
        learnConfigParser.showUsage
        sys.exit(1)
    }

    val bestTrans = new util.ArrayList[Transformation](config.nIter)

    logger.trace("Start learning...")
    for (
      i <- (1 to config.nIter).iterator;
      transOp = TransformationExtractor.findBestTransformation(trainAlignTrees)
      if transOp.isDefined;
      trans = transOp.get
    ) {
      bestTrans.add(trans)

      trainAlignTrees = trainAlignTrees.map(trans(_))
      val newTrainScore = trainAlignTrees.map(_.agreementScore).sum

      devAlignTreesOp match {
        case None =>
          logger.info(s"Iteration $i:\t$trans\tTrain Score: $newTrainScore")

        case Some(devAlignTrees) =>
          devAlignTreesOp = Some(devAlignTrees.map(trans(_)))
          val newDevScore = devAlignTreesOp.get.map(_.agreementScore).sum
          logger.info(s"Iteration $i:\t$trans\tTrain Score: $newTrainScore\tDev Score: $newDevScore")
      }

      if (i % 20 == 0 && config.outPath.isDefined) {
        logger.info("Saving transformation sequence up till now...")
        writeBestTrans(bestTrans, config.outPath)
      }
    }

    writeBestTrans(bestTrans, config.outPath)
  }

  def writeBestTrans(bestTrans: util.ArrayList[Transformation], outPathOp: Option[String]) {
    val xStream = getXStream()

    outPathOp match {
      case Some(outPath) =>
        val writer = new PrintWriter(outPath, "UTF-8")
        xStream.toXML(bestTrans, writer)
        writer.close()
        logger.info("Result saved to " + outPath)
      case None =>
        println(xStream.toXML(bestTrans))
    }
  }

  def getXStream(): XStream = {
    val xStream = new XStream()

    xStream.setMode(XStream.NO_REFERENCES)

    xStream.alias("Optional", classOf[Option[(String, Direction)]])
    xStream.alias("Some", classOf[Some[(String, Direction)]])
    xStream.alias("None", None.getClass)
    xStream.alias("Pair", classOf[(String, Direction)])

    xStream.alias("Direction", classOf[Direction])
    xStream.alias("Left", Left.getClass)
    xStream.alias("Right", Right.getClass)
    xStream.alias("Transformation", classOf[Transformation])

    import trans._
    xStream.alias("Adopt", classOf[Adopt])
    xStream.alias("Articulate", classOf[Articulate])
    xStream.alias("Demote", classOf[Demote])
    xStream.alias("Flatten", classOf[Flatten])
    xStream.alias("Promote", classOf[Promote])
    xStream.alias("Transfer", classOf[Transfer])

    xStream
  }

  def applyMain(args: Seq[String]) {
    val config = applyConfigParser.parse(args, ApplyConfig(null, null, null)).getOrElse {
      sys.exit(1)
    }

    val transformations = {
      val array =
        getXStream().fromXML(new File(config.transPath)).asInstanceOf[util.ArrayList[Transformation]]
      config.nTrans match {
        case Some(nTrans) =>
          array.toVector.take(nTrans)
        case None =>
          array.toVector
      }
    }

    def applyAllTrans(tree: SyntaxTree): SyntaxTree =
      transformations.foldLeft(tree)((currTree, trans) => trans(currTree))

    val writer = new PrintWriter(config.outTreePath, "UTF-8")

    val lineIter = io.Source.fromFile(config.inTreePath).getLines()

    val chunkSize = 20000
    var counter = 0
    while (!lineIter.isEmpty) {
      val lineChunk = lineIter.take(chunkSize).toVector.par
      lineChunk.map(line =>
        if (line.trim.isEmpty) ""
        else applyAllTrans(SyntaxTree.parse(line)).toString
      ).seq.foreach(writer.println _)
      counter += lineChunk.length
      logger.info(s"$counter processed")
    }

    writer.close()
  }
}
