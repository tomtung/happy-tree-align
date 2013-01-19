package edu.isi.nlg.happytreealign

import collection.immutable.Queue
import util.parsing.combinator.RegexParsers
import SyntaxTree.Node
import annotation.tailrec

class SyntaxTree private(val root: Node,
                         val parentOf: Map[Node, Node],
                         val spanOf: Map[Node, Span]) {
  def traverseBreadthFirst: List[Node] = root.traverseBreadthFirst

  def traverseLeftRightBottomUp: List[Node] = root.traverseLeftRightBottomUp

  def traversePostOrder: List[Node] = root.traversePostOrder

  def replace(oldNode: Node, newNode: Node): SyntaxTree = {
    @tailrec
    def getUpdatedRoot(oldNode: Node, newNode: Node): Node =
      parentOf.get(oldNode) match {
        case None => newNode // replacing the root
        case Some(parent) =>
          val i = parent.children.indexOf(oldNode)
          val updatedChildren = (parent.children.slice(0, i) :+ newNode) ++ parent.children.slice(i + 1, parent.children.length)
          val updatedParent = new Node(parent.label, updatedChildren)
          getUpdatedRoot(parent, updatedParent)
      }

    val updatedRoot = getUpdatedRoot(oldNode, newNode)
    SyntaxTree(updatedRoot)
  }

  override lazy val toString: String = "( " + root.toString + " )"
}

object SyntaxTree {

  class Node(val label: String,
             val children: Vector[Node] = Vector.empty[Node],
             val isMerged: Boolean = false) {
    def isLeaf = children.isEmpty

    override lazy val toString: String = {
      if (children.isEmpty) label
      else "(%s %s)".format(label, children.iterator.map(_.toString).mkString(" "))
    }

    lazy val traversePostOrder: List[Node] = {
      @tailrec
      def doTraverse(stack: List[(Node, Boolean)], accu: List[Node]): List[Node] = {
        stack match {
          case Nil => accu.reverse
          case (head, true) :: tail =>
            doTraverse(tail, head :: accu)
          case (head, false) :: tail =>
            if (head.isLeaf)
              doTraverse(tail, head :: accu)
            else {
              val nextNodes = head.children.iterator.map(_ -> false).toList
              doTraverse(nextNodes ::: ((head, true) :: tail), accu)
            }
        }
      }

      doTraverse(List(this -> false), Nil)
    }

    lazy val traverseBreadthFirst: List[Node] = {
      @tailrec
      def doTraverse(queue: Queue[Node], accu: List[Node]): List[Node] =
        if (queue.isEmpty) accu.reverse
        else {
          val (h, q) = queue.dequeue
          doTraverse(q.enqueue(h.children), h :: accu)
        }

      doTraverse(Queue(this), Nil)
    }

    lazy val traverseLeftRightBottomUp: List[Node] = {
      @tailrec
      def doTraverse(queue: Queue[Node], accu: List[Node]): List[Node] =
        if (queue.isEmpty) accu
        else {
          val (h, q) = queue.dequeue
          doTraverse(q.enqueue(h.children.reverseIterator.toList), h :: accu)
        }

      doTraverse(Queue(this), Nil)
    }
  }

  def apply(root: Node): SyntaxTree = {
    val (parentOf, spanOf) = {
      @tailrec
      def build(nodes: List[Node],
                parentOf: Map[Node, Node],
                spanOf: Map[Node, NonEmptySpan],
                leafCount: Int): (Map[Node, Node], Map[Node, NonEmptySpan]) =
        nodes match {
          case Nil =>
            (parentOf, spanOf)
          case head :: tail =>
            if (head.isLeaf) {
              val span = NonEmptySpan(leafCount, leafCount)
              build(tail, parentOf, spanOf + (head -> span), leafCount + 1)
            } else {
              val span = {
                val chSpans = head.children.map(spanOf)
                val from = chSpans.minBy(_.from).from
                val to = chSpans.maxBy(_.to).to
                NonEmptySpan(from, to)
              }
              build(tail, parentOf ++ head.children.iterator.map(_ -> head), spanOf + (head -> span), leafCount)
            }
        }

      build(root.traversePostOrder, Map.empty, Map.empty, 0)
    }


    new SyntaxTree(root, parentOf, spanOf)
  }

  private object TreeStructureParser extends RegexParsers {

    def identifier: Parser[String] = regex( """[^\(\)\s]+""".r)

    def atom: Parser[Node] = identifier ^^ {
      s => new Node(s)
    }

    def list: Parser[Node] = "(" ~> identifier ~ rep(sExpr) <~ ")" ^^ {
      case root ~ ch => new Node(root, ch.toVector)
    }

    def sExpr: Parser[Node] = atom | list

    def start: Parser[Node] = sExpr | "(" ~> sExpr <~ ")"

    def apply(s: String): Node = {
      parse(start, s) match {
        case Success(result, _) => result
        case NoSuccess(msg, _) =>
          throw new ParsingException("Invalid tree string: " + s + "; " + msg)
      }
    }
  }

  def parse(s: String): SyntaxTree = SyntaxTree(TreeStructureParser(s))
}
