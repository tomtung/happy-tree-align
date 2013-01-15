package edu.isi.nlg.happytreealign

import scala.collection.immutable.SortedSet

class WordAlignments(val fToE: Map[Int, SortedSet[Int]], val eToF: Map[Int, SortedSet[Int]])

object WordAlignments {
  def fromFEPairs(fEPairs: Seq[(Int, Int)]): WordAlignments = {
    val fToE = fEPairs.groupBy(_._1).mapValues(seq => SortedSet[Int]() ++ seq.map(_._2))
    val eToF = fEPairs.groupBy(_._2).mapValues(seq => SortedSet[Int]() ++ seq.map(_._1))
    new WordAlignments(fToE, eToF)
  }

  def parseFEPairs(s: String): WordAlignments = {
    val pairs = s.trim.split("\\s+").map(ps => {
      val splited = ps.split('-')
      if (splited.length != 2)
        throw new ParsingException("Invalid alignment string: " + s)
      try {
        splited(0).toInt -> splited(1).toInt
      } catch {
        case e: Throwable =>
          throw new ParsingException("Invalid alignment string: " + s, e)
      }
    })

    fromFEPairs(pairs)
  }
}
