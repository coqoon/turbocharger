package dk.itu.turbocharger.java.ui

import dk.itu.turbocharger.parsing.DecoratedDocument
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.text.{IRegion, IDocument}
import org.eclipse.jface.text.reconciler.{
  DirtyRegion, IReconcilingStrategy, IReconcilingStrategyExtension}
import org.eclipse.jdt.core.dom.{TypeDeclaration, CompilationUnit}
import dk.itu.turbocharger.java.DecoratedJavaDocument

class DecoratedJavaReconcilingStrategy(
    editor : DecoratedJavaEditor) extends IReconcilingStrategy {
  import org.eclipse.jdt.core.JavaCore
  import org.eclipse.jdt.core.dom.{AST, ASTParser}

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
        val doc = new DecoratedJavaDocument(p.getTokens)
        val javaView = doc.getJavaView
        val parser = ASTParser.newParser(AST.JLS3)
        parser.setSource(javaView.get.toCharArray)
        parser.setProject(JavaCore.create(f.getFile.getProject))
        parser.setResolveBindings(true)

        import scala.collection.JavaConversions._
        Option(parser.createAST(null)) match {
          case Some(cu : CompilationUnit) =>
            for (i <- cu.types.flatMap(TryCast[TypeDeclaration]);
                 m <- i.getMethods) {
              val body = m.getBody
              val viewRegion =
                Region(body.getStartPosition, length = body.getLength)
              val region = javaView.toSingleDocumentRegion(viewRegion)
              println(s"The body of '${m.getName}' contains the following complete tokens:")
              println(s"\t${doc.getTokens(region)}")
            }
            import org.eclipse.core.resources.{IMarker, IResource}
            val file = TryCast[FileEditorInput](editor.getEditorInput)
            file.foreach(_.getFile.deleteMarkers(
                dk.itu.coqoon.core.ManifestIdentifiers.MARKER_PROBLEM, false, IResource.DEPTH_ZERO))
            for (m <- cu.getMessages;
                 r @ Region(start, length) <- javaView.toDocumentRegions(
                     Region(m.getStartPosition, length = m.getLength))) {
              import scala.collection.JavaConversions._
              val mark = f.getFile.createMarker(
                  dk.itu.coqoon.core.ManifestIdentifiers.MARKER_PROBLEM)
              println("Created marker " + mark)
              mark.setAttributes(Map(
                  (IMarker.MESSAGE, m.getMessage),
                  (IMarker.SEVERITY, IMarker.SEVERITY_ERROR),
                  (IMarker.LOCATION, s"offset ${start}"),
                  (IMarker.CHAR_START, start),
                  (IMarker.CHAR_END, r.end),
                  (IMarker.TRANSIENT, true)))
            }
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
  def rts(r : IRegion) =
    s"IRegion(length=${r.getLength}, offset=${r.getOffset})"
  def drts(dr : DirtyRegion) =
    s"DirtyRegion(length=${dr.getLength}, offset=${dr.getOffset}, " +
    s"text=${dr.getText}, type=${dr.getType})"
}
