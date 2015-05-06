package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.core.ManifestIdentifiers.MARKER_PROBLEM
import dk.itu.turbocharger.java.DecoratedJavaDocument
import dk.itu.turbocharger.parsing.DecoratedDocument
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.text.{IRegion, IDocument}
import org.eclipse.jface.text.reconciler.{
  DirtyRegion, IReconcilingStrategy, IReconcilingStrategyExtension}
import org.eclipse.jdt.core.dom.{TypeDeclaration, CompilationUnit}


class DecoratedJavaReconcilingStrategy(
    editor : DecoratedJavaEditor) extends IReconcilingStrategy {
  import org.eclipse.jdt.core.JavaCore
  import org.eclipse.jdt.core.dom.{AST, Message, ASTParser}

  import dk.itu.turbocharger.coq._
  /* XXX: this is probably a very bad way of getting a name */
  private val dp = new DispatchPool(editor.getEditorInput.getName)

  import DecoratedDocument.Region
  import DecoratedJavaReconcilingStrategy._
  override def reconcile(r : IRegion) =
    println(s"${this}.reconcile(${rts(r)})")
  override def reconcile(dr : DirtyRegion, r : IRegion) = {
    println(s"${this}.reconcile(${drts(dr)}, ${rts(r)})")
    import dk.itu.coqoon.core.utilities.TryCast
    import org.eclipse.ui.part.FileEditorInput
    val input = TryCast[FileEditorInput](editor.getEditorInput)
    (input, editor.partitioner) match {
      case (Some(f), Some(p)) =>
        import dk.itu.turbocharger.java.DecoratedJavaCoqDocument
        import DecoratedJavaCoqDocument.{UnsupportedException, generateCompletePIDEDocument}
        val doc = new DecoratedJavaDocument(f.getFile, p.getTokens)

        val pideDoc =
          try {
            Right(generateCompletePIDEDocument(doc))
          } catch {
            case e : UnsupportedException =>
              Left(e)
          }
        val syntheticMessages = pideDoc.left.toOption.map(e => new Message(
            e.message, e.node.getStartPosition, e.node.getLength)).toSeq
        pideDoc.right.foreach(doc => {
          dp.defineDocument(doc.map(_.toString))
        })

        val javaView = doc.getJavaView

        import scala.collection.JavaConversions._
        Option(doc.getCompilationUnit) match {
          case Some(cu : CompilationUnit) =>
            import dk.itu.turbocharger.java.ASTUtilities.children
            import org.eclipse.jdt.core.dom._
            for (t <- children[TypeDeclaration](cu);
                 m <- children[MethodDeclaration](t)) {
              val body = m.getBody
              val viewRegion =
                Region(body.getStartPosition, length = body.getLength)
              val region = javaView.toSingleDocumentRegion(viewRegion)
              println(s"The body of '${m.getName}' contains the following partial tokens:")
              println(s"\t${doc.getPartialTokens(region)}")
            }
            import org.eclipse.core.resources.{IMarker, IResource}
            val file = TryCast[FileEditorInput](editor.getEditorInput)
            file.foreach(_.getFile.deleteMarkers(
                MARKER_PROBLEM, false, IResource.DEPTH_ZERO))
            (cu.getMessages ++ syntheticMessages).foreach(
                m => spreadError(
                    f.getFile, m.getMessage,
                    Region(m.getStartPosition, length = m.getLength),
                    javaView))
          case _ =>
            None
        }
      case _ =>
    }
  }

  private var document : Option[IDocument] = None
  override def setDocument(d: IDocument) =
    document = Option(d)
}
object DecoratedJavaReconcilingStrategy {
  import org.eclipse.jdt.core.dom.Message
  import org.eclipse.core.resources.{IFile, IMarker}
  import DecoratedDocument.Region
  /* Creates one or more markers to represent an error message pertaining to a
   * particular region in a typed view. (If the given region crosses a type
   * boundary, more than one marker will be created.) */
  def spreadError(f : IFile,
      s : String, r : Region, v : DecoratedDocument#TypedView) = {
    for (r @ Region(start, length) <- v.toDocumentRegions(r)) yield {
      import scala.collection.JavaConversions._
      val mark = f.createMarker(MARKER_PROBLEM)
      println("Created marker " + mark)
      mark.setAttributes(Map(
          (IMarker.MESSAGE, s),
          (IMarker.SEVERITY, IMarker.SEVERITY_ERROR),
          (IMarker.LOCATION, s"offset ${start}"),
          (IMarker.CHAR_START, start),
          (IMarker.CHAR_END, r.end),
          (IMarker.TRANSIENT, true)))
      mark
    }
  }
  def rts(r : IRegion) =
    s"IRegion(length=${r.getLength}, offset=${r.getOffset})"
  def drts(dr : DirtyRegion) =
    s"DirtyRegion(length=${dr.getLength}, offset=${dr.getOffset}, " +
    s"text=${dr.getText}, type=${dr.getType})"
}
