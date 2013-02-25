package edu.isi.nlg.happytreealign

sealed trait Direction

object Direction {

  case object Left extends Direction

  case object Right extends Direction

}

