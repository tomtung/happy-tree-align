package edu.isi.nlg.happytreealign

import SyntaxTree.Node

case class ArticulateTrans(parentLabel: String,
                           leftLabel: String,
                           rightLabel: String) extends Transformation {

  override protected def applyOnAnchorNode(parent: Node): Option[Node] = {
    import ArticulateTrans._

    if (parent.isLeaf || parent.label != parentLabel || freshlyArticulated(parent))
      return None

    val children = parent.children
    val idxOp = (0 until children.length - 1).find(
      i => {
        val left = children(i)
        val right = children(i + 1)
        !left.isMerged && !right.isMerged &&
          left.label == leftLabel && right.label == rightLabel
      }
    )

    idxOp.map(i => {
      val left = children(i)
      val right = children(i + 1)
      val mergedNode = {
        val mergedLabel = createMergedLabel(left, right)
        val mergedChildren = Vector(left, right)
        new Node(mergedLabel, mergedChildren, true)
      }

      val updatedParent = {
        val updatedChildren = children.patch(i, List(mergedNode), 2)
        new Node(parent.label, updatedChildren)
      }

      updatedParent
    })
  }
}

object ArticulateTrans extends TransformationExtractor {

  private def createMergedLabel(left: Node, right: Node): String = left.label + "+" + right.label

  private def freshlyArticulated(node: Node): Boolean =
    !node.isLeaf && node.isMerged && node.children.length == 2 && {
      val l = node.children(0)
      val r = node.children(1)
      node.label == createMergedLabel(l, r)
    }

  override protected def extractAtAnchorNode(parent: Node): TraversableOnce[Transformation] = {
    if (parent.children.length < 2 || freshlyArticulated(parent)) Traversable.empty
    else {
      def chIter = parent.children.iterator
      for (
        (l, r) <- chIter zip chIter.drop(1)
        if !l.isMerged && !r.isMerged
      ) yield ArticulateTrans(parent.label, l.label, r.label)
    }
  }
}
