package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class PromoteTrans(grandparentLabel: String,
                        parentLabel: String,
                        childLabel: String,
                        direction: Direction) extends Transformation {
  override protected def applyOnAnchorNode(grandparent: Node): Option[Node] = {
    if (grandparent.isLeaf || grandparent.label != grandparentLabel)
      return None

    val parent = direction match {
      case Left => grandparent.children.last
      case Right => grandparent.children.head
    }

    if (parent.label != parentLabel || parent.isLeaf)
      return None

    val child = direction match {
      case Left => parent.children.head
      case Right => parent.children.last
    }

    if (child.label != childLabel)
      return None

    val patch =
      if (parent.children.length == 1)
        List(parent.children.head)
      else direction match {
        case Left => List(
          parent.children.head,
          new Node(parent.label, parent.children.drop(1))
        )
        case Right => List(
          new Node(parent.label, parent.children.dropRight(1)),
          parent.children.last
        )
      }

    val updatedChildren = direction match {
      case Left =>
        grandparent.children.patch(grandparent.children.length - 1, patch, 1)
      case Right =>
        grandparent.children.patch(0, patch, 1)
    }
    Some(new Node(grandparentLabel, updatedChildren))
  }
}

object PromoteTrans extends TransformationExtractor {
  override protected def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    if (grandparent.isLeaf) return Nil

    var lst: List[PromoteTrans] = Nil

    {
      val leftParent = grandparent.children.head
      if (!leftParent.isLeaf) {
        val child = leftParent.children.last
        lst = PromoteTrans(grandparent.label, leftParent.label, child.label, Right) :: lst
      }
    }

    {
      val rightParent = grandparent.children.last
      if (!rightParent.isLeaf) {
        val child = rightParent.children.head
        lst = PromoteTrans(grandparent.label, rightParent.label, child.label, Left) :: lst
      }
    }

    lst
  }
}
