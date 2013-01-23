import collection.BitSet
import java.io.PrintWriter

def printUsageAndExit() {
  System.err.println("Usage: scala sample_aligntrees.scala <number> <tree-path> <align-path>")
  sys.exit(1)
}

if (args.length != 3)
  printUsageAndExit()

val nSample = args(0).toInt
def treeSource = io.Source.fromFile(args(1), "UTF-8")
def alignSource = io.Source.fromFile(args(2), "UTF-8")

if (treeSource.getLines().length != alignSource.getLines().length) {
  System.err.println("Error: line number doesn't match")
  printUsageAndExit()
}

def linePairs = (treeSource.getLines() zip alignSource.getLines()).filter({
  case (tl, al) => !tl.isEmpty && !al.isEmpty && al.split(' ').length > 5
})
val nLines = linePairs.length

if (nSample >= nLines) {
  System.err.println("Error: number of samples too large")
  printUsageAndExit()
}

val selectedLineNums =
  BitSet() ++
    util.Random.shuffle(
      (0 until nLines).toIndexedSeq
    ).iterator.take(nSample)

val selectedLinePairs =
  util.Random.shuffle(
    linePairs.zipWithIndex.filter({
      case (_, i) =>
        selectedLineNums.contains(i)
    }).map(_._1).toIndexedSeq
  )

val treeSampleWriter = new PrintWriter(args(1) + ".sample", "UTF-8")
val alignSampleWriter = new PrintWriter(args(2) + ".sample", "UTF-8")

selectedLinePairs.foreach({
  case (tl, al) =>
    treeSampleWriter.println(tl)
    alignSampleWriter.println(al)
})

treeSampleWriter.close()
alignSampleWriter.close()
