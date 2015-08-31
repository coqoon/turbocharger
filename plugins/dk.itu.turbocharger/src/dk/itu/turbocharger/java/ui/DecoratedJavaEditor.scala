package dk.itu.turbocharger.java.ui

import dk.itu.turbocharger.java.{Partitioning, DecoratedJavaPartitioner}

import org.eclipse.ui.editors.text.{
  TextEditor, TextFileDocumentProvider, ForwardingDocumentProvider}
import org.eclipse.core.filebuffers.IDocumentSetupParticipant

class DecoratedJavaEditor extends TextEditor {
  protected def createSourceViewerConfiguration() =
    new DecoratedJavaEditorSourceViewerConfiguration(this)

  private[ui] val session = new dk.itu.coqoon.ui.pide.SessionManager
  session.start

  override protected def dispose() = {
    session.stop
    super.dispose
  }

  private val reconciler = new DecoratedJavaReconciler(this)

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.IVerticalRuler
  override protected def createSourceViewer(
      parent : Composite, ruler : IVerticalRuler, styles : Int) = {
    val viewer = super.createSourceViewer(parent, ruler, styles)
    reconciler.install(viewer)
    viewer
  }

  protected[ui] var partitioner : Option[DecoratedJavaPartitioner] = None

  private object DecoratedJavaDocumentSetupParticipant
      extends IDocumentSetupParticipant {
    import org.eclipse.jface.text.IDocument
    override def setup(doc : IDocument) =
      partitioner =
        Some(DecoratedJavaPartitioner.installPartitioner(doc, Partitioning.ID))
  }

  override protected def initializeEditor() = {
    setDocumentProvider(new ForwardingDocumentProvider(
      Partitioning.ID, DecoratedJavaDocumentSetupParticipant,
      new TextFileDocumentProvider {
        override def getDefaultEncoding() = "UTF-8"
      }))
    setSourceViewerConfiguration(createSourceViewerConfiguration)
    super.initializeEditor
  }
}
