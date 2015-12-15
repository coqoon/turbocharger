package dk.itu.turbocharger.java

import dk.itu.turbocharger.text.TokeniserPartitioner
import org.eclipse.jface.text.{IDocument, IDocumentExtension3}

class DecoratedJavaPartitioner extends TokeniserPartitioner(
    DecoratedJavaTokeniser, JavaRecogniser.States.java,
    DecoratedJavaPartitioner.mapping)
object DecoratedJavaPartitioner {
  import dk.itu.turbocharger.coq.CoqRecogniser
  private[DecoratedJavaPartitioner] val mapping = {
    import Partitioning._
    import CoqRecogniser.States._
    import JavaRecogniser.States._
    Map(java -> Java.ContentTypes.JAVA,
        javaChar -> Java.ContentTypes.CHAR,
        javaString -> Java.ContentTypes.STRING,
        javaMultiLineComment -> Java.ContentTypes.COMMENT,
        javaSingleLineComment -> Java.ContentTypes.COMMENT,
        coq -> Coq.ContentTypes.COQ,
        coqString -> Coq.ContentTypes.STRING,
        coqComment -> Coq.ContentTypes.COMMENT)
  }

  def installPartitioner(input : IDocument, partitioning : String) = {
    import dk.itu.coqoon.core.utilities.TryCast
    val partitioner = createPartitioner
    TryCast[IDocumentExtension3](input) match {
      case Some(ext) =>
        ext.setDocumentPartitioner(partitioning, partitioner)
      case None =>
        input.setDocumentPartitioner(partitioner)
    }
    partitioner.connect(input)
    partitioner
  }

  def createPartitioner() = new DecoratedJavaPartitioner
}
