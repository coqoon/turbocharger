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

  import isabelle.{Text, Command, Session, Protocol, Document}

  import dk.itu.coqoon.ui.utilities.SupersedableTask
  val uiMoveTask = new SupersedableTask(200)

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) =
      uiMoveTask schedule {
        caretPing
      }
  }

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

  private var lastCommand : Option[Command] = None

  import dk.itu.coqoon.ui.pide.Responses
  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val caret = Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset)
      val commandResultsAndMarkup = caret.flatMap(caret =>
        executeWithCommandsLock {
          val c = findCommand(caret)
          lastSnapshot.flatMap(snapshot => c.map(
              c => (c,
                  Responses.extractResults(snapshot, c._2),
                  Responses.extractMarkup(snapshot, c._2))))
        })
      commandResultsAndMarkup match {
        case Some(((offset, command), results, markup)) =>
          val sameCommand = lastCommand.contains(command)

          markup.find(_._2.name == "goals") match {
            case Some((_, el))
                if !sameCommand || goals == None =>
              setGoals(Responses.extractGoals(el))
            case Some(el) => /* do nothing? */
            case None => setGoals(None)
          }

          if (!sameCommand) {
            import dk.itu.coqoon.ui.utilities.EclipseConsole
            for ((_, tree) <- results) {
              Responses.extractWritelnMessage(tree).foreach(message =>
                  EclipseConsole.out.println(message))
              Responses.extractError(tree).foreach(error =>
                  EclipseConsole.err.println(error._2))
            }
            lastCommand = Option(command)
          }
        case _ =>
          setGoals(None)
          lastCommand = None
      }
    }

  import org.eclipse.jface.text.source.Annotation
  private var annotations : Map[Command, Seq[Annotation]] = Map()

  override protected def commandsUpdated(changed : Seq[Command]) = {
    import dk.itu.coqoon.ui.pide.Responses
    val changedResultsAndMarkup =
      (executeWithCommandsLock {
        val ls = lastSnapshot.get
        lastSnapshot.foreach(snapshot =>
            commands =
              (for (command <- snapshot.node.commands;
                    offset <- snapshot.node.command_start(command))
                yield (offset, command)).toSeq)
        for (c <- changed)
          yield {
            (ls.node.command_start(c), c,
                Responses.extractResults(ls, c),
                Responses.extractMarkup(ls, c))
          }
      })

    asyncExec {
      val am =
        if (CoqoonUIPreferences.ProcessingAnnotations.get) {
          getAnnotationModel
        } else None
      am.foreach(
          model => Option(getViewer).map(_.getDocument).foreach(model.connect))

      import org.eclipse.jface.text.Position
      var toDelete : Seq[(Command, Seq[Annotation])] = Seq()
      var annotationsToAdd : Seq[(Command, Seq[(Annotation, Position)])] = Seq()
      var errorsToAdd : Seq[(Command, (Int, Int), String)] = Seq()
      var errorsToDelete : Seq[Command] = Seq()

      try {
        for (i <- changedResultsAndMarkup) i match {
          case (Some(offset), command, results, markup) =>
            val posMap = (pideDocument.find(_._1 == offset) map {
              case (_, (_, map)) => map
            })
            val complete =
              !(Protocol.Status.make(
                  markup.map(_._2.markup).iterator).is_running)

            /* Extract and display error messages */
            var commandHasErrors = false
            for ((_, tree) <- results) {
              (getFile, Responses.extractError(tree), posMap) match {
                case (Some(f), Some((id, msg, Some(start), Some(end))),
                      Some(map)) =>
                  val r = Region(start - 1, length = (end - start))
                  for ((region, po) <- map;
                       f <- region.intersection(r);
                       nr = f.translate(po - region.start)) {
                    errorsToAdd :+= (command, (nr.start, nr.end), msg)
                  }
                  commandHasErrors = true
                case (Some(f), Some((id, msg, _, _)), Some(map)) =>
                  for ((region, po) <- map;
                       nr = region.translate(po))
                    errorsToAdd :+= (command, (nr.start, nr.end), msg)
                  commandHasErrors = true
                case _ =>
              }
            }
            if (!commandHasErrors)
              errorsToDelete :+= command

            val old = annotations.get(command).toSeq.flatten
            val makeNew = !complete

            if (!old.isEmpty)
              toDelete :+= (command, old)
            (makeNew, posMap) match {
              case (true, Some(map)) =>
                annotationsToAdd :+= (command, map.toSeq map {
                  case (r, offset) =>
                    (new Annotation(
                         ManifestIdentifiers.Annotations.PROCESSING,
                         false, "Processing proof"),
                     r.translate(offset).makePosition)
                })
              case _ =>
            }
          case (None, command, _, _) =>
            /* This command has been removed from the document; delete its
             * annotation, along with any errors it might have */
            toDelete :+= (command, annotations.get(command).toSeq.flatten)
        }
      } finally {
        am.foreach(model => {
          import scala.collection.JavaConversions._
          val del =
            (for ((command, ans) <- toDelete)
              yield {
                annotations -= command
                ans
              }).flatten
          val add =
            (for ((command, ans) <- annotationsToAdd)
              yield {
                annotations += (command -> ans.map(_._1))
                ans
              }).flatten.toMap
          model.replaceAnnotations(del.toArray, add)

          model.disconnect(getViewer.getDocument)
          getSourceViewer.invalidateTextPresentation
        })

        if (!toDelete.isEmpty || !errorsToAdd.isEmpty ||
            !errorsToDelete.isEmpty)
          getFile.foreach(file =>
            new dk.itu.coqoon.ui.pide.UpdateErrorsJob(file,
                commands,
                toDelete.map(_._1) ++ errorsToDelete,
                errorsToAdd).schedule)
      }

      lastCommand match {
        case None =>
          caretPing
        case Some(c) if changed.contains(c) =>
          caretPing
        case _ =>
      }
    }
  }
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
    viewer.getTextWidget.addCaretListener(DocumentCaretListener)
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

  override def findCommand(offset : Int) : Option[(Int, Command)] = {
    for ((docOffset, (command, regions)) <- pideDocument) {
      for ((cr, po) <- regions if cr.translate(po).contains(offset))
        return commands.find(_._1 == docOffset)
    }
    None
  }
}
