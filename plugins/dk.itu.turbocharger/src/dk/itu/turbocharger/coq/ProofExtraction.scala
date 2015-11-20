package dk.itu.turbocharger.coq

import dk.itu.coqoon.core.coqtop.CoqSentence.getNextSentences
import dk.itu.turbocharger.parsing.DecoratedDocument
import org.eclipse.jdt.core.dom.MethodDeclaration
import DecoratedDocument.Region

object ProofExtraction {
  private final val Precondition = """^<%\s*precondition:""".r.unanchored
  private final val Postcondition = """^<%\s*postcondition:""".r.unanchored
  /* @sr is the region in which to look for specifications, @pr the region for
   * proofs, @doc the document containing those regions, @langView the
   * language-specific view of that document, and @coqView the Coq-specific
   * one. */
  def extractProof(
      method : MethodDeclaration,
      sr : DecoratedDocument.Region,
      pr : DecoratedDocument.Region,
      doc : DecoratedDocument,
      langView : DecoratedDocument#TypedView,
      coqView : DecoratedDocument#TypedView) = {
    val (pre, post) =
      doc.getPartialTokens(sr) match {
        case Some((start, tokens)) =>
          import dk.itu.turbocharger.java.Partitioning

          val tp = DecoratedDocument.withPositions(
              start, tokens).filter(t => coqView.contains(t._2))
          /* XXX: This extraction discards position information, probably
           * interacts badly with comments, and is generally in need of being
           * replaced with something cleverer */
          val pre = tp.collectFirst {
            case (o, (pt, a @ Precondition()))
                if pt.label.startsWith(Partitioning.Coq.ContentTypes.COQ) =>
              a.substring(2, a.length - 2).trim.stripPrefix("precondition:")
          }
          val post = tp.collectFirst {
            case (o, (pt, a @ Postcondition()))
                if pt.label.startsWith(Partitioning.Coq.ContentTypes.COQ) =>
              a.substring(2, a.length - 2).trim.stripPrefix("postcondition:")
          }
          (pre.map(ArbitraryTerm), post.map(ArbitraryTerm))
        case _ =>
          (None, None)
      }
    println(pre, post)

    if (!pre.isEmpty || !post.isEmpty) {
      /* This method has a precondition, postcondition, or both. Extract
       * the proof that it (hopefully!) satisfies them */
      val pt =
        doc.getPartialTokens(pr) match {
          case Some((start, tokens)) =>
            DecoratedDocument.withPositions(
                start, tokens).filter(t => coqView.contains(t._2))
          case _ =>
            Seq()
        }

      import dk.itu.coqoon.core.utilities.Substring
      import isabelle.Command_Span
      val sentences = pt flatMap {
        case (start, (token, content)) =>
          /* Strip the leading and trailing antiquote bits from this token
           * and extract all the sentences that it contains */
          var pos = 2
          import dk.itu.turbocharger.coq.CommonSyntax.parse_spans
          val spans = parse_spans(Substring(content, 2, content.length - 2))
          (for (Command_Span.Span(kind, body) <- spans)
            yield {
              val c = body.map(_.source).mkString
              try {
                if (kind != Command_Span.Ignored_Span) {
                  Some(ArbitrarySentence(c.toString),
                      Map(Region(0, length = c.length) -> (start + pos)))
                } else None
              } finally pos += c.length
            }).flatten
      }
      /* XXX: copy-and-paste from
       * DecoratedJavaDocument.generateDefinitionsForType */
      val b = method.resolveBinding
      val definitionId =
        s"${b.getDeclaringClass.getName}_${b.getName}"

      ((Theorem(s"${definitionId}_s", ConstructorInvocation2(
          "lentails",
          IdentifierTerm("ltrue"),
          ConstructorInvocation3(
              "triple",
              pre.getOrElse(ArbitraryTerm("False")),
              IdentifierTerm(s"${definitionId}_body"),
              post.getOrElse(ArbitraryTerm("False"))))), Map.empty[Region, Int])) +:
          ((Theorem.Proof, Map.empty[Region, Int]) +: sentences :+
           (Theorem.Qed, Map.empty[Region, Int]))
    } else Seq()
  }
}