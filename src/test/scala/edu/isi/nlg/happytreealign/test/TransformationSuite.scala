package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._
import Direction._
import scala.Some

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
    assert(ArticulateTrans.extract(tree) === Set(
      ArticulateTrans("a", "b", "c"),
      ArticulateTrans("a", "c", "x"),
      ArticulateTrans("a", "x", "b")
    ))

    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")(tree).get
    assert(ArticulateTrans.extract(newTree) === Set.empty)
  }

  test("applying flatten transformation (example in original paper)") {
    val tree = SyntaxTree.parse("( (NP (DT the) (NNP China) (NML (NNP Trade) (NNP Promotion)) (NNP Council)) )")
    val newTree: SyntaxTree = FlattenTrans("NP", "NML")(tree).get

    assert(newTree.toString === "( (NP (DT the) (NNP China) (NNP Trade) (NNP Promotion) (NNP Council)) )")
  }

  test("applying flatten transformation (test for exhaustion)") {
    val tree = SyntaxTree.parse("( (a (b (a (b a b)) b) (b a b)) )")
    val newTree: SyntaxTree = FlattenTrans("a", "b")(tree).get

    assert(newTree.toString === "( (a (a a b) b a b) )")
  }

  test("extract flatten transformations (both with and without context)") {
    val tree = SyntaxTree.parse("( (a l (b (d g h i) e) (c (f j k) m)) )")
    assert(FlattenTrans.extract(tree) === Set(
      FlattenTrans("a", "b"),
      FlattenTrans("a", "b", Some(("l", Right))),
      FlattenTrans("a", "b", Some(("c", Left))),
      FlattenTrans("c", "f"),
      FlattenTrans("c", "f", Some(("m", Left))),
      FlattenTrans("a", "c"),
      FlattenTrans("a", "c", Some(("b", Right)))
    ))
  }

  test("applying flatten transformation with context (example in original paper)") {
    val tree = SyntaxTree.parse("( (NP (DT the) (NNP1 China) (NML (NNP Trade) (NNP Promotion)) (NNP2 Council)) )")

    assert(
      FlattenTrans("NP", "NML", Some(("NNP1", Right)))(tree).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")
    assert(
      FlattenTrans("NP", "NML", Some(("NNP2", Left)))(tree).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(FlattenTrans("NP", "NML", Some(("NNP1", Left)))(tree).isEmpty)
    assert(FlattenTrans("NP", "NML", Some(("NNP2", Right)))(tree).isEmpty)
  }

  test("applying promote transformation") {
    val tree1 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP1 N. Taugia))) )")
    assert(
      PromoteTrans("PP", "NP", "NP", Left)(tree1).get.toString ===
        "( (PP (IN by) (NP the French player) (NP (NP1 N. Taugia))) )")

    val tree2 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(
      PromoteTrans("NP", "NP", "player", Right)(tree2).get.toString ===
        "( (PP (IN by) (NP (NP the French) player (NP N. Taugia))) )")

    val tree3 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(
      PromoteTrans("PP", "NP", "NP", Left)(tree3).get.toString ===
        "( (PP (IN by) (NP the French player) (NP N. Taugia)) )")

    assert(PromoteTrans("PP", "NP", "NP", Right)(tree3).isEmpty)
  }

  test("extract promote transformations") {
    val tree = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(PromoteTrans.extract(tree) === Set(
      PromoteTrans("PP", "IN", "by", Right),
      PromoteTrans("PP", "NP", "NP", Left),
      PromoteTrans("NP", "NP", "player", Right),
      PromoteTrans("NP", "NP", "N.", Left)
    ))
  }
}
