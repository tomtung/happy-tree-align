package edu.isi.nlg.happytreealign

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation]

  // TODO extract a protected extractFromAnchorNode, and provide a basic implementation of extract?
  // see current implementation of PromoteTrans.extract
}
