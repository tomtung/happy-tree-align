package edu.isi.nlg.happytreealign

import util.parsing.combinator.RegexParsers
import SyntaxTree.Node
import annotation.tailrec
import collection.mutable

class SyntaxTree(val root: Node) {

  lazy val (parentOf, spanOf) = {
    val parentOfBuilder = Map.newBuilder[Node, Node]
    val spanOfBuilder = Map.newBuilder[Node, NonEmptySpan]
    val spanOf = mutable.Map[Node, NonEmptySpan]()
    var leafCount = 0

    val nNodes = root.traversePostOrder.length
    parentOfBuilder.sizeHint(nNodes)
    spanOfBuilder.sizeHint(nNodes)
    spanOf.sizeHint(nNodes)

    @tailrec
    def build(nodes: List[Node] = root.traversePostOrder): (Map[Node, Node], Map[Node, NonEmptySpan]) =
      nodes match {
        case Nil =>
          (parentOfBuilder.result(), spanOfBuilder.result())
        case head :: tail =>
          if (head.isLeaf) {
            val span = NonEmptySpan(leafCount, leafCount)
            spanOf += (head -> span)
            spanOfBuilder += (head -> span)
            leafCount += 1
            build(tail)
          } else {
            val span = {
              val chSpans = head.children.map(spanOf)
              val from = chSpans.minBy(_.from).from
              val to = chSpans.maxBy(_.to).to
              NonEmptySpan(from, to)
            }
            parentOfBuilder ++= head.children.iterator.map(_ -> head)
            spanOf += (head -> span)
            spanOfBuilder += (head -> span)
            build(tail)
          }
      }

    build()
  }


  def traverseBreadthFirst: List[Node] = root.traverseBreadthFirst

  def traverseLeftRightBottomUp: List[Node] = root.traverseLeftRightBottomUp

  def traversePostOrder: List[Node] = root.traversePostOrder

  def replace(oldNode: Node, newNode: Node): SyntaxTree = {
    if (oldNode != root && !parentOf.contains(oldNode)) this
    else {
      @tailrec
      def getUpdatedRoot(oldNode: Node, newNode: Node): Node =
        parentOf.get(oldNode) match {
          case None => newNode // replacing the root
          case Some(parent) =>
            val i = parent.children.indexOf(oldNode)
            val updatedChildren = parent.children.patch(i, List(newNode), 1)
            getUpdatedRoot(parent, parent.childrenUpdated(updatedChildren))
        }

      val updatedRoot = getUpdatedRoot(oldNode, newNode)
      new SyntaxTree(updatedRoot)
    }
  }

  override lazy val toString: String = "( " + root.toString + " )"
}

object SyntaxTree {

  class Node(val label: String,
             val children: Vector[Node] = Vector.empty[Node],
             val isMerged: Boolean = false) {
    def isLeaf = children.isEmpty

    def childrenUpdated(updatedChildren: Vector[Node]) = new Node(label, updatedChildren, isMerged)

    lazy val isPos = !isLeaf && children.forall(_.isLeaf)

    override lazy val toString: String = {
      if (children.isEmpty) label
      else "(%s %s)".format(label, children.iterator.map(_.toString).mkString(" "))
    }

    lazy val traversePostOrder: List[Node] = {
      val stack = mutable.Stack[(Node, Boolean)](this -> false)
      val builder = List.newBuilder[Node]

      @tailrec
      def doTraverse(): List[Node] = {
        if (stack.isEmpty) builder.result()
        else {
          stack.pop() match {
            case (head, true) =>
              builder += head
              doTraverse()
            case (head, false) if head.isLeaf =>
              builder += head
              doTraverse()
            case (head, false) =>
              stack.push(head -> true)
              stack.pushAll(head.children.reverseIterator.map(_ -> false))
              doTraverse()
          }
        }
      }

      doTraverse()
    }

    lazy val traverseBreadthFirst: List[Node] = {
      val queue = mutable.Queue(this)

      @tailrec
      def doTraverse(accu: List[Node] = Nil): List[Node] =
        if (queue.isEmpty) accu.reverse
        else {
          val head = queue.dequeue()
          head.children.foreach(queue.enqueue(_))
          doTraverse(head :: accu)
        }

      doTraverse()
    }

    lazy val traverseLeftRightBottomUp: List[Node] = {
      val queue = mutable.Queue[Node](this)

      @tailrec
      def doTraverse(accu: List[Node] = Nil): List[Node] =
        if (queue.isEmpty) accu
        else {
          val head = queue.dequeue()
          head.children.reverseIterator.foreach(node => queue.enqueue(node))
          doTraverse(head :: accu)
        }

      doTraverse()
    }
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

  def parse(s: String): SyntaxTree = new SyntaxTree(TreeStructureParser(s))
}
