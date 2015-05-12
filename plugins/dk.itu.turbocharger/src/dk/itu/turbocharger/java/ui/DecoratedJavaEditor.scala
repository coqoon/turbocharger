package dk.itu.turbocharger.java.ui

import dk.itu.turbocharger.java.{Partitioning, DecoratedJavaPartitioner}

import org.eclipse.ui.editors.text.{
  TextEditor, TextFileDocumentProvider, ForwardingDocumentProvider}
import org.eclipse.core.filebuffers.IDocumentSetupParticipant

class DecoratedJavaEditor extends TextEditor {
  protected def createSourceViewerConfiguration() =
    new DecoratedJavaEditorSourceViewerConfiguration(this)

  private[ui] val session = new dk.itu.coqoon.ui.pide.SessionManager

  override protected def dispose() = {
    session.stop
    super.dispose
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
