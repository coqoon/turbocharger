package dk.itu.turbocharger.coq

object CommonSyntax {
  private val syntax = new isabelle.Coq_Syntax

  def parse_spans(input : CharSequence) = syntax.parse_spans(input)
}