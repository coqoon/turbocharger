package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.ui.{
  CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}
import dk.itu.coqoon.ui.pide.{Perspective, PIDESessionHost}
import dk.itu.turbocharger.java.{Partitioning, DecoratedJavaPartitioner}

import org.eclipse.ui.editors.text.{
  TextEditor, TextFileDocumentProvider, ForwardingDocumentProvider}
import org.eclipse.core.filebuffers.IDocumentSetupParticipant

class DecoratedJavaEditor
    extends TextEditor with CoqGoalsContainer with PIDESessionHost {
  protected def createSourceViewerConfiguration() =
    new DecoratedJavaEditorSourceViewerConfiguration(this)

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jface.text.source.AnnotationModel
  protected def getAnnotationModel() = Option(getDocumentProvider).flatMap(
      p => Option(p.getAnnotationModel(getEditorInput)).flatMap(
          TryCast[AnnotationModel]))

  override protected def dispose() = {
    session.stop
    super.dispose
  }

  def getViewer = super.getSourceViewer

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.IFile
  import isabelle.{Text, Command, Session, Document}
  /* XXX: copied-and-pasted from PIDECoqEditor... */
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  /* ... apart from the trailing ".v", required by Coq */
  override protected[ui] def getNodeName() =
    getFile.map(file => Document.Node.Name(file.getName + ".v"))

  /* XXX: exposing this lock to the reconciler can't possibly be a good idea */
  override protected[ui] def executeWithCommandsLock[A](f : => A) =
    super.executeWithCommandsLock(f)

  import dk.itu.turbocharger.coq.CoqCommand
  import dk.itu.turbocharger.parsing.DecoratedDocument
  import dk.itu.turbocharger.parsing.DecoratedDocument.Region
  /* Seq((position in the generated PIDE document, Option[(Coq command, region
   * in the complete document corresponding to the Coq command)])) */
  private[ui] var pideDocument :
      Seq[(Int, (CoqCommand, Map[Region, Int]))] = Seq()
  private[ui] var commands : Seq[(Int, Command)] = Seq()

  import org.eclipse.jface.text.source.Annotation
  private var annotations : Map[Command, Annotation] = Map()

  override protected def commandsUpdated(changed : Seq[Command]) =
    println(s"$this.commandsUpdated($changed)")
  import dk.itu.coqoon.ui.utilities.UIUtils.exec
  import dk.itu.coqoon.core.utilities.TotalReader
  override protected def generateInitialEdits() = exec {
    val fi = TryCast[IFileEditorInput](getEditorInput)
    (fi, partitioner) match {
      case (Some(f), Some(p)) =>
        differ.makeEdits(List())
        import dk.itu.turbocharger.java.{
          DecoratedJavaDocument, DecoratedJavaCoqDocument}
        val doc = new DecoratedJavaDocument(f.getFile, p.getTokens)
        val pdoc = DecoratedJavaCoqDocument.generateCompletePIDEDocument(doc)
        val text = pdoc.map(_._1.toString.trim + "\n").toList
        List[Document.Node.Edit[Text.Edit, Text.Perspective]](
            Document.Node.Clear(),
            Document.Node.Edits(differ.makeEdits(text)),
            getPerspective)
      case _ =>
        List()
    }
  }

  override protected def getPerspective() = {
    val overlay = getOverlay match {
      case Some((o, _)) => o.wrap
      case _ => Document.Node.Overlays.empty
    }
    Perspective.makeFull(overlay)
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

  import dk.itu.turbocharger.coq.PIDEDiff
  private[ui] lazy val differ = new PIDEDiff

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

  override def findCommand(offset : Int) = None
}
