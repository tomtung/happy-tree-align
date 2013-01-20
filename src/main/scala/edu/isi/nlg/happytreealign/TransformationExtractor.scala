package edu.isi.nlg.happytreealign

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation]
}
