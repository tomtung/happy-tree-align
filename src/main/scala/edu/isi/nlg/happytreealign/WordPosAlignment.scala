package edu.isi.nlg.happytreealign

import scala.collection.immutable.SortedSet
import scala.collection.concurrent
import math.{max, min}

class WordPosAlignment private(val fToE: Map[Int, SortedSet[Int]], val eToF: Map[Int, SortedSet[Int]]) {
  private def mapSpan(span: Span, posMap: Map[Int, SortedSet[Int]], spanMap: concurrent.Map[Span, Span]) = {
    def computeMatchingSpan() = span match {
      case EmptySpan => EmptySpan
      case NonEmptySpan(_, _) =>
        val allMatchedIter = span.range.iterator.collect(posMap)
        if (allMatchedIter.isEmpty) EmptySpan
        else {
          val (from, to) = allMatchedIter.foldLeft((Int.MaxValue, Int.MinValue))(
            (pair, set) =>
              (min(pair._1, set.min), max(pair._2, set.max))
          )
          Span(from, to)
        }
    }

    spanMap.getOrElseUpdate(span, computeMatchingSpan())
  }

  private val _fSpanToESpan = concurrent.TrieMap[Span, Span]()

  def fSpanToESpan(span: Span): Span = mapSpan(span, fToE, _fSpanToESpan)

  private val _eSpanToFSpan = concurrent.TrieMap[Span, Span]()

  def eSpanToFSpan(span: Span): Span = mapSpan(span, eToF, _eSpanToFSpan)
}

object WordPosAlignment {
  def fromFEPairs(fEPairs: Traversable[(Int, Int)]): WordPosAlignment = {
    val fToE = fEPairs.groupBy(_._1).mapValues(seq => SortedSet[Int]() ++ seq.map(_._2))
    val eToF = fEPairs.groupBy(_._2).mapValues(seq => SortedSet[Int]() ++ seq.map(_._1))
    new WordPosAlignment(fToE, eToF)
  }

  def parseFEPairs(s: String): WordPosAlignment = {
    val pairs = s.trim.split("\\s+").map(ps => {
      val split = ps.split('-')
      if (split.length != 2)
        throw new ParsingException(message = "Invalid alignment string: " + s)
      try {
        split(0).toInt -> split(1).toInt
      } catch {
        case e: Exception =>
          throw new ParsingException(message = "Invalid alignment string: " + s, cause = e)
      }
    })

    fromFEPairs(pairs)
  }
}
