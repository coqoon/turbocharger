package dk.itu.turbocharger.java

import org.eclipse.jface.text.{IDocument, IDocumentExtension3}
import org.eclipse.jface.text.{TypedRegion, ITypedRegion}
import org.eclipse.jface.text.{
  DocumentEvent, IDocumentPartitioner, IDocumentPartitionerExtension2}

import dk.itu.coqoon.core.utilities.CacheSlot

class DecoratedJavaPartitioner
    extends IDocumentPartitioner with IDocumentPartitionerExtension2 {
  private val tokens = CacheSlot {
    DecoratedJavaTokeniser.tokenise(this.document.get.get).toList
  }

  def getTokens() = tokens.get
  def getCoqView() = getTokens.filter(
      _._1.label.startsWith(Partitioning.Coq.ContentTypes.COQ))
  def getJavaView() = getTokens.filter(
      _._1.label.startsWith(Partitioning.Java.ContentTypes.JAVA))

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
  override def documentAboutToBeChanged(ev: DocumentEvent) : Unit =
    println(s"${this}.documentAboutToBeChanged(${ev})")
  override def documentChanged(ev : DocumentEvent) : Boolean = {
    tokens.clear
    true
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
    println(s"${this}.getPartition(${offset}, ${withEmptyPartitions})")
    var pos = 0
    for ((t, s) <- tokens.get) {
      val end = pos + s.length
      if (offset >= pos &&
          (offset < end || document.map(_.getLength).contains(offset))) {
        return new TypedRegion(pos, s.length, t.label)
      } else pos = end
    }
    return null
  }
}
object DecoratedJavaPartitioner {
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
