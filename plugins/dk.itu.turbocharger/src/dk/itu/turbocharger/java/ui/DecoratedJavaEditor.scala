package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.ui.CoqGoalsContainer
import dk.itu.turbocharger.java.{Partitioning, DecoratedJavaPartitioner}

import org.eclipse.ui.editors.text.{
  TextEditor, TextFileDocumentProvider, ForwardingDocumentProvider}
import org.eclipse.core.filebuffers.IDocumentSetupParticipant

class DecoratedJavaEditor
    extends TextEditor with CoqGoalsContainer {
  protected def createSourceViewerConfiguration() =
    new DecoratedJavaEditorSourceViewerConfiguration(this)

  private[ui] val session = new dk.itu.coqoon.ui.pide.SessionManager
  session.start

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.IFile
  import isabelle.{Command, Session, Document}
  /* XXX: copied-and-pasted from PIDECoqEditor... */
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  /* ... apart from the trailing ".v", required by Coq */
  protected[ui] def getNodeName() =
    getFile.map(file => Document.Node.Name(file.getName + ".v"))

  /* XXX: exposing this lock to the reconciler can't possibly be a good idea */
  private[ui] object CommandsLock
  import dk.itu.turbocharger.coq.CoqCommand
  import dk.itu.turbocharger.parsing.DecoratedDocument
  /* Seq((position in the generated PIDE document, Option[(Coq command, region
   * in the complete document corresponding to the Coq command)])) */
  private[ui] var pideDocument :
      Seq[(Int, (CoqCommand, Option[DecoratedDocument.Region]))] = Seq()
  private var lastSnapshot : Option[Document.Snapshot] = None
  private[ui] var commands : Seq[(Int, Command)] = Seq()

  private def commandsUpdated(changed : Seq[Command]) =
    asyncExec {
      import dk.itu.coqoon.ui.pide.Responses
      val changedResultsAndMarkup =
        (CommandsLock synchronized {
          val ls = lastSnapshot.get
          for (c <- changed)
            yield (ls.node.command_start(c), c,
                      Responses.extractResults(ls, c),
                      Responses.extractMarkup(ls, c))
        })
      println(changedResultsAndMarkup)
    }

  session.addInitialiser(session =>
    session.commands_changed += Session.Consumer[Any]("Coqoon") {
      case changed : Session.Commands_Changed =>
        CommandsLock synchronized {
          lastSnapshot = getNodeName.map(n => session.snapshot(n))
          lastSnapshot.foreach(snapshot =>
            commands =
              (for (command <- snapshot.node.commands;
                    offset <- snapshot.node.command_start(command))
                yield (offset, command)).toSeq)
        }
        commandsUpdated(changed.commands.toSeq)
      case _ =>
    })

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
