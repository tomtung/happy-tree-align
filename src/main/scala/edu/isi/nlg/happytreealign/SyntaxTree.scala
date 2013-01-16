package edu.isi.nlg.happytreealign

import collection.immutable.{Queue, IndexedSeq}
import util.parsing.combinator.RegexParsers
import SyntaxTree.Node
import collection.mutable

class SyntaxTree private(val root: Node,
                         val parentOf: Map[Node, Node],
                         val spanOf: Map[Node, Span],
                         val isFromMerge: Set[Node]) {

  def traverseBreadthFirst: List[Node] = root.traverseBreadthFirst

  def traverseLeftRightBottomUp: List[Node] = root.traverseLeftRightBottomUp

  def traversePostOrder: List[Node] = root.traversePostOrder
}

object SyntaxTree {

  class Node(val label: String,
             val children: IndexedSeq[Node] = IndexedSeq.empty[Node]) {
    lazy val traversePostOrder: List[Node] = {
      (children.map(_.traversePostOrder).flatten :+ this).toList
    }

    lazy val traverseBreadthFirst: List[Node] = {
      def doTraverse(queue: Queue[Node], accu: List[Node]): List[Node] =
        if (queue.isEmpty) accu
        else {
          val (h, q) = queue.dequeue
          doTraverse(q.enqueue(h.children), h :: accu)
        }

      doTraverse(Queue(this), Nil).reverse
    }

    lazy val traverseLeftRightBottomUp: List[Node] = {
      def doTraverse(queue: Queue[Node], accu: List[Node]): List[Node] =
        if (queue.isEmpty) accu
        else {
          val (h, q) = queue.dequeue
          doTraverse(q.enqueue(h.children.reverse), h :: accu)
        }

      doTraverse(Queue(this), Nil)
    }
  }

  def apply(root: Node): SyntaxTree = {
    val (parentOf, spanOf) = {
      val parentOf = mutable.Map[Node, Node]()
      val spanOf = mutable.Map[Node, Span]()
      def build(nodes: List[Node] = root.traversePostOrder,
                leafCount: Int = 0) {
        nodes match {
          case node :: rest =>
            if (node.children.isEmpty) {
              val span = Span(leafCount, leafCount)
              spanOf += (node -> span)
              build(rest, leafCount + 1)
            } else {
              val span = {
                val chSpans = node.children.map(spanOf)
                val from = chSpans.minBy(_.from).from
                val to = chSpans.maxBy(_.to).to
                Span(from, to)
              }
              spanOf += (node -> span)
              parentOf ++= node.children.map(_ -> node)
              build(rest, leafCount)
            }
          case Nil =>
        }
      }

      build()
      (parentOf.toMap, spanOf.toMap)
    }


    new SyntaxTree(root, parentOf, spanOf, Set.empty)
  }

  private object TreeStructureParser extends RegexParsers {

    def identifier: Parser[String] = regex( """[^\(\)\s]+""".r)

    def atom: Parser[Node] = identifier ^^ {
      s => new Node(s)
    }

    def list: Parser[Node] = "(" ~> identifier ~ rep(sExpr) <~ ")" ^^ {
      case root ~ ch => new Node(root, ch.toIndexedSeq)
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
