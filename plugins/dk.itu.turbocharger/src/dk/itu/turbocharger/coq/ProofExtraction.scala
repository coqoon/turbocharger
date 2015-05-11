package dk.itu.turbocharger.coq

import dk.itu.coqoon.core.coqtop.CoqSentence.getNextSentences
import dk.itu.turbocharger.parsing.DecoratedDocument
import DecoratedDocument.Region

object ProofExtraction {
  /* @sr is the region in which to look for specifications, @pr the region for
   * proofs, @doc the document containing those regions, @langView the
   * language-specific view of that document, and @coqView the Coq-specific
   * one. */
  def extractProof(
      sr : DecoratedDocument.Region,
      pr : DecoratedDocument.Region,
      doc : DecoratedDocument,
      langView : DecoratedDocument#TypedView,
      coqView : DecoratedDocument#TypedView) = {
    var pre : Option[ArbitraryTerm] = None
    var post : Option[ArbitraryTerm] = None
    doc.getPartialTokens(sr) match {
      case Some((start, tokens)) =>
        DecoratedDocument.withPositions(
            start, tokens).filter(t => coqView.contains(t._2))
      case _ =>
        Seq()
    }

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

      pt flatMap {
        case (start, (token, content)) =>
          /* Strip the leading and trailing antiquote bits from this token
           * and extract all the sentences that it contains */
          var pos = 2
          for ((c, s) <- getNextSentences(content, 2, content.length - 2))
            yield {
              try {
                (ArbitrarySentence(c.toString),
                    Some(Region(start + pos, length = c.length)))
              } finally pos += c.length
            }
      }
    } else Seq()
  }
}