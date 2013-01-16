package edu.isi.nlg.happytreealign

sealed trait Span {
  def range: Range
}

case object EmptySpan extends Span {
  lazy val range: Range = Range(0, 0)
}

case class NonEmptySpan(from: Int, to: Int) extends Span {
  if (from > to || from < 0 || to < 0)
    throw new IllegalArgumentException("Illegal argument: from = %d, to = %d".format(from, to))

  lazy val range = Range(from, to + 1)
}

object Span {
  def apply(): Span = EmptySpan

  def apply(from: Int, to: Int): Span = NonEmptySpan(from, to)
}