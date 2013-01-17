package edu.isi.nlg.happytreealign

sealed trait Span {
  def range: Range

  def contains(span: Span): Boolean

  def contains(pos: Int): Boolean

  def length: Int
}

case object EmptySpan extends Span {
  lazy val range: Range = Range(0, 0)

  override def contains(span: Span): Boolean = span == EmptySpan

  override def contains(pos: Int): Boolean = false

  override val length: Int = 0
}

case class NonEmptySpan(from: Int, to: Int) extends Span {
  if (from > to || from < 0 || to < 0)
    throw new IllegalArgumentException("Illegal argument: from = %d, to = %d".format(from, to))

  override lazy val range = Range(from, to + 1)

  override def contains(span: Span): Boolean = span match {
    case EmptySpan => true
    case NonEmptySpan(f, t) => contains(f) && contains(t)
  }

  override def contains(pos: Int): Boolean = pos >= from && pos <= to

  override val length: Int = to - from + 1
}

object Span {
  def apply(): Span = EmptySpan

  def apply(from: Int, to: Int): Span = NonEmptySpan(from, to)
}
