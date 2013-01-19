package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign.{SyntaxTree, ArticulateTrans}

class TransformationSuite extends FunSuite {
  test("applying articulate transformation (example in original paper)") {
    val tree = SyntaxTree.parse("( (S (NP Other members) (VP will arrive in two groups) (. .)) )")
    val newTree = ArticulateTrans(parentLabel = "S", leftLabel = "NP", rightLabel = "VP")(tree).get

    assert(newTree.toString === "( (S (NP+VP (NP Other members) (VP will arrive in two groups)) (. .)) )")
  }

  test("applying articulate transformation (test for exhaustion)") {
    val tree = SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")
    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")(tree).get

    assert(newTree.toString === "( (a (b+c (b x) (c (a (b+c b c)))) x (b+c b c)) )")
  }

  test("apply articulate transformation (fail to apply)") {
    val articulate = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")
    val tree1 = SyntaxTree.parse("( (a (b x) (d (a b d)) x b d) )")
    val newTree1Op = articulate(tree1)
    assert(newTree1Op.isEmpty)

    val tree2 = articulate(SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")).get
    val newTree2Op = ArticulateTrans(parentLabel = "a+b", leftLabel = "a", rightLabel = "b")(tree2)
    assert(newTree2Op.isEmpty)
  }

  test("extract articulate transformations") {
    val tree = SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")
    assert(ArticulateTrans.extractFrom(tree) === Set(
      ArticulateTrans("a", "b", "c"),
      ArticulateTrans("a", "c", "x"),
      ArticulateTrans("a", "x", "b")
    ))

    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")(tree).get
    assert(ArticulateTrans.extractFrom(newTree) === Set.empty)
  }

}
