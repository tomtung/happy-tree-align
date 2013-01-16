package edu.isi.nlg.happytreealign

case class Span(from: Int, to: Int) {
  if (from > to || from < 0 || to < 0)
    throw new IllegalArgumentException("Illegal argument: from = %d, to = %d".format(from, to))

  lazy val range = Range(from, to + 1)
}
