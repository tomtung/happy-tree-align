package edu.isi.nlg.happytreealign

import scala.collection.immutable.SortedSet

class WordPosAlignment private(val fToE: Map[Int, SortedSet[Int]], val eToF: Map[Int, SortedSet[Int]])

object WordPosAlignment {
  def fromFEPairs(fEPairs: Seq[(Int, Int)]): WordPosAlignment = {
    val fToE = fEPairs.groupBy(_._1).mapValues(seq => SortedSet[Int]() ++ seq.map(_._2))
    val eToF = fEPairs.groupBy(_._2).mapValues(seq => SortedSet[Int]() ++ seq.map(_._1))
    new WordPosAlignment(fToE, eToF)
  }

  def parseFEPairs(s: String): WordPosAlignment = {
    val pairs = s.trim.split("\\s+").map(ps => {
      val split = ps.split('-')
      if (split.length != 2)
        throw new ParsingException("Invalid alignment string: " + s)
      try {
        split(0).toInt -> split(1).toInt
      } catch {
        case e: Throwable =>
          throw new ParsingException("Invalid alignment string: " + s, e)
      }
    })

    fromFEPairs(pairs)
  }
}
