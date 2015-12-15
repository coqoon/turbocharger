package dk.itu.turbocharger.java

import org.eclipse.jface.text.{IDocument, IDocumentExtension3}
import org.eclipse.jface.text.{IRegion, ITypedRegion}
import org.eclipse.jface.text.{DocumentEvent, IDocumentPartitioner,
  IDocumentPartitionerExtension, IDocumentPartitionerExtension2,
  IDocumentPartitionerExtension3}

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.parsing.{Tokeniser, PushdownAutomaton}
import dk.itu.turbocharger.parsing.DecoratedDocument.Region

object DocumentAdapter {
  class DocumentSequence(
      d : IDocument, start : Int, end : Int) extends CharSequence {
    final lazy val length = end - start
    override def charAt(pos : Int) = d.getChar(start + pos)
    override def subSequence(start : Int, end : Int) =
      new DocumentSequence(d, start + start, start + end)
  }
  def makeSequence(d : IDocument) : CharSequence =
    new DocumentSequence(d, 0, d.getLength)
}

class TokeniserDrivenPartitioner(
    t : Tokeniser, start : PushdownAutomaton.State,
    mapping : Map[PushdownAutomaton.State, String])
    extends IDocumentPartitioner
        with IDocumentPartitionerExtension
        with IDocumentPartitionerExtension2
        with IDocumentPartitionerExtension3 {
  private val tokens = CacheSlot {
    t.tokens(start, DocumentAdapter.makeSequence(this.document.get)).toList
  }

  def getTokens() = tokens.get

  /* Defer to the IDocumentPartitionerExtension version of this method */
  override def documentChanged(ev : DocumentEvent) =
    (documentChanged2(ev) != null)
  /* Defer to the IDocumentPartitionerExtension2 versions of these methods */
  override def computePartitioning(
      offset : Int, length : Int) = computePartitioning(offset, length, false)
  override def getPartition(offset : Int) = getPartition(offset, false)
  override def getContentType(offset : Int) = getContentType(offset, false)
  /* Defer to the IDocumentPartitionerExtension3 version of this method */
  override def connect(document : IDocument) : Unit = connect(document, false)

  private var document : Option[IDocument] = None

  override def connect(
      document : IDocument, delayInitialisation : Boolean) : Unit = {
    this.document = Option(document)
    if (!delayInitialisation)
      getTokens()
  }
  override def disconnect() : Unit = {
    this.document = None
    tokens.clear
  }

  import org.eclipse.jface.text.DocumentRewriteSession
  private var session : Option[DocumentRewriteSession] = None
  override def startRewriteSession(session : DocumentRewriteSession) =
    this.session match {
      case Some(s) =>
        throw new IllegalStateException("Rewrite session already underway")
      case None =>
        this.session = Option(session)
    }
  override def stopRewriteSession(session : DocumentRewriteSession) =
    this.session match {
      case Some(s) if s == session =>
        this.session = None
      case _ =>
    }
  override def getActiveRewriteSession() = session.orNull

  override def documentAboutToBeChanged(ev: DocumentEvent) : Unit = ()
  /* This is a bit dodgy: we return the correct changed region of the document,
   * but we completely destroy all of the cached tokens in the process! (We
   * have to destroy at least some of the tokens to stop them becoming stale,
   * though...) */
  override def documentChanged2(ev : DocumentEvent) : IRegion =
    tokens.asOption match {
      case Some(ts) =>
        tokens.clear
        var pos = 0
        for ((t, s) <- ts) {
          val tr = Region(pos, length = s.length)
          if (tr.contains(ev.fOffset)) {
            return tr.resize(ev.fDocument.getLength - pos)
          } else pos = tr.end
        }
        null
      case None =>
        /* To err on the safe side, claim that everything's changed */
        Region(0, length = ev.fDocument.getLength)
    }

  private final lazy val contentTypes = mapping.values.toSet.toArray
  override def getLegalContentTypes() = contentTypes

  override def computePartitioning(offset : Int, length : Int,
      withEmptyPartitions : Boolean) : Array[ITypedRegion] = {
    var (i, end) = (offset, offset + length)
    var s = Seq[ITypedRegion]()
    while (i < end) {
      val p = getPartition(i, withEmptyPartitions)
      if (!s.contains(p))
        s :+= p
      i = p.getOffset + p.getLength
    }
    s.toArray
  }
  override def getContentType(offset : Int,
      withEmptyPartitions : Boolean) : String =
    getPartition(offset, withEmptyPartitions).getType
  override def getManagingPositionCategories() : Array[String] = {
    null /* XXX: should we stash the partition information in the document? */
  }
  override def getPartition(
      offset : Int, withEmptyPartitions : Boolean) : ITypedRegion = {
    var pos = 0
    for ((t, s) <- getTokens if withEmptyPartitions || !s.isEmpty;
         label <- mapping.get(t)) {
      val tr = Region(pos, length = s.length)
      if (tr.contains(offset) || document.map(_.getLength).contains(offset)) {
        return tr.asTypedRegion(label)
      } else pos = tr.end
    }
    return null
  }
}

class DecoratedJavaPartitioner extends TokeniserDrivenPartitioner(
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
