package dk.itu.turbocharger.java.ui

import dk.itu.turbocharger.ManifestIdentifiers.Markers.JAVA_PROBLEM
import dk.itu.coqoon.ui.EventReconciler
import dk.itu.coqoon.ui.text.Region
import dk.itu.turbocharger.java.DecoratedJavaDocument
import dk.itu.turbocharger.text.DecoratedDocument
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.text.{IRegion, IDocument}
import org.eclipse.jdt.core.dom.{TypeDeclaration, CompilationUnit}

class DecoratedJavaReconciler(
    editor : DecoratedJavaEditor) extends EventReconciler {
  import DecoratedJavaReconciler._

  import org.eclipse.jdt.core.JavaCore
  import org.eclipse.jdt.core.dom.{AST, Message, ASTParser}

  import EventReconciler.DecoratedEvent
  override def process(events : List[DecoratedEvent]) = {
    println(s"${this}.reconcile($events)")
    import dk.itu.coqoon.core.utilities.TryCast
    import org.eclipse.ui.part.FileEditorInput
    val input = TryCast[FileEditorInput](editor.getEditorInput)
    (input, editor.partitioner) match {
      case (Some(f), Some(p)) =>
        import dk.itu.turbocharger.java.JavaDefinitions.UnsupportedException
        import dk.itu.turbocharger.java.DecoratedJavaCoqDocument
        import DecoratedJavaCoqDocument.generateCompletePIDEDocument
        val doc = new DecoratedJavaDocument(f.getFile, p.getTokens)

        val pideDoc =
          try {
            Right(generateCompletePIDEDocument(doc))
          } catch {
            case e : UnsupportedException =>
              Left(e)
          }
        pideDoc.right.foreach(h => print(h.mkString("\n")))

        /* XXX: this used to be synchronised on the commands lock; should we
         * synchronise on something else now that the lock is hidden? */
        import dk.itu.turbocharger.coq.CoqCommand
        val d = pideDoc.right.toOption.getOrElse(Seq())
        /* Before you ask, the + 1 is for the line separator that appears in
         * the generated PIDE document. Yes, I hate it too */
        editor.pideDocument =
          DecoratedDocument.withPositions[(CoqCommand, Map[Region, Int])](
              _._1.toString.trim.length + 1)(0, d.toStream)

        val syntheticMessages = pideDoc.left.toOption.map(e => new Message(
            s"${e.node.getClass.getSimpleName}: ${e.message}",
            e.node.getStartPosition, e.node.getLength)).toSeq
        pideDoc.right.foreach(doc => {
          val rawEdits = editor.differ.makeEdits(
              doc.map(_._1.toString.trim + "\n").toList)
          editor.getNodeName.foreach(nodeName => {
            import isabelle.Document
            editor.checkedUpdate(List(Document.Node.Edits(rawEdits)))
          })
        })

        val javaView = doc.getJavaView

        import scala.collection.JavaConversions._
        Option(doc.getCompilationUnit).foreach(cu => {
          import org.eclipse.core.resources.{IMarker, IResource}
          val file = TryCast[FileEditorInput](editor.getEditorInput)
          file.foreach(_.getFile.deleteMarkers(
              JAVA_PROBLEM, false, IResource.DEPTH_ZERO))
          (cu.getMessages ++ syntheticMessages).foreach(
              m => spreadError(
                  f.getFile, m.getMessage,
                  Region(m.getStartPosition, length = m.getLength),
                  javaView))
        })
      case _ =>
    }
  }
}
object DecoratedJavaReconciler {
  import org.eclipse.jdt.core.dom.Message
  import org.eclipse.core.resources.{IFile, IMarker}
  /* Creates one or more markers to represent an error message pertaining to a
   * particular region in a typed view. (If the given region crosses a type
   * boundary, more than one marker will be created.) */
  def spreadError(f : IFile,
      s : String, r : Region, v : DecoratedDocument#TypedView) = {
    for (r @ Region(start, length) <- v.toDocumentRegions(r)) yield {
      import scala.collection.JavaConversions._
      val mark = f.createMarker(JAVA_PROBLEM)
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
}
