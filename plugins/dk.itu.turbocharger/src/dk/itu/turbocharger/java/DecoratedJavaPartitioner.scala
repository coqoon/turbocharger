package dk.itu.turbocharger.java

import org.eclipse.jface.text.{IDocument, IDocumentExtension3}
import org.eclipse.jface.text.{IRegion, TypedRegion, ITypedRegion}
import org.eclipse.jface.text.{DocumentEvent, IDocumentPartitioner,
  IDocumentPartitionerExtension, IDocumentPartitionerExtension2}

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.parsing.{Tokeniser, PushdownAutomaton}

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
    t : Tokeniser, start : PushdownAutomaton[Char]#Execution,
    mapping : Map[PushdownAutomaton.State, String])
    extends IDocumentPartitioner with IDocumentPartitionerExtension
        with IDocumentPartitionerExtension2 {
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

  private var document : Option[IDocument] = None

  override def connect(document : IDocument) : Unit = {
    this.document = Option(document)
    tokens.get
  }
  override def disconnect() : Unit = {
    this.document = None
  }

  override def documentAboutToBeChanged(ev: DocumentEvent) : Unit = ()
  override def documentChanged2(ev : DocumentEvent) : IRegion = {
    tokens.clear
    dk.itu.turbocharger.parsing.DecoratedDocument.Region(
        0, length = ev.fDocument.getLength)
  }
  override def getLegalContentTypes() = Partitioning.TYPES

  override def computePartitioning(offset : Int, length : Int,
      withEmptyPartitions : Boolean) : Array[ITypedRegion] = {
    println(s"${this}.computePartitioning(${offset}, ${length}, ${withEmptyPartitions})")
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
    /*println(s"${this}.getManagingPositionCategories()")
    return Array.empty*/
    null /* XXX: should we stash the partition information in the document? */
  }
  override def getPartition(
      offset : Int, withEmptyPartitions : Boolean) : ITypedRegion = {
    var pos = 0
    for ((t, s) <- tokens.get;
         label <- mapping.get(t)) {
      val end = pos + s.length
      if (offset >= pos &&
          (offset < end || document.map(_.getLength).contains(offset))) {
        return new TypedRegion(pos, s.length, label)
      } else pos = end
    }
    return null
  }
}

class DecoratedJavaPartitioner extends TokeniserDrivenPartitioner(
    DecoratedJavaTokeniser,
    DecoratedJavaRecogniser.Execution(JavaRecogniser.States.java, Seq()),
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
