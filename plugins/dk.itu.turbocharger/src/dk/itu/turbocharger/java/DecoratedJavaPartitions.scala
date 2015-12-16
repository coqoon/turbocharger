package dk.itu.turbocharger.java

object DecoratedJavaPartitions {
  final val ID = "__turbo_dj"
  object Types {
    final val JAVA = s"${ID}_java"
    final val JAVA_CHAR = s"${JAVA}_char"
    final val JAVA_STRING = s"${JAVA}_string"
    final val JAVA_COMMENT = s"${JAVA}_comment"

    final val COQ = s"${ID}_coq"
    final val COQ_STRING = s"${COQ}_string"
    final val COQ_COMMENT = s"${COQ}_comment"
  }
  final lazy val TYPES = mapping.values.toSet.toArray

  import dk.itu.coqoon.ui.text.coq.CoqRecogniser
  private[DecoratedJavaPartitions] lazy val mapping = {
    import CoqRecogniser.States._
    import JavaRecogniser.States._
    Map(java -> Types.JAVA,
        javaChar -> Types.JAVA_CHAR,
        javaString -> Types.JAVA_STRING,
        javaMultiLineComment -> Types.JAVA_COMMENT,
        javaSingleLineComment -> Types.JAVA_COMMENT,
        coq -> Types.COQ,
        coqString -> Types.COQ_STRING,
        coqComment -> Types.COQ_COMMENT)
  }

  import org.eclipse.jface.text.{IDocument, IDocumentExtension3}
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

  import dk.itu.coqoon.ui.text.TokeniserPartitioner
  def createPartitioner() = new TokeniserPartitioner(
    DecoratedJavaTokeniser, JavaRecogniser.States.java, mapping)
}
