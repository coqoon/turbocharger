package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.ui.{
  CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}
import dk.itu.coqoon.ui.pide.{Perspective, PIDESessionHost}
import dk.itu.coqoon.ui.text.TokeniserPartitioner
import dk.itu.turbocharger.java.DecoratedJavaPartitions

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

  import dk.itu.coqoon.ui.text.Region
  import dk.itu.turbocharger.coq.CoqCommand
  import dk.itu.turbocharger.text.DecoratedDocument
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
      val ls = getLastSnapshot
      val caret = Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset)
      val commandResultsAndMarkup = caret.flatMap(caret => {
        val c = findCommand(caret)
        ls.flatMap(snapshot => c.map(
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
    val ls = getLastSnapshot.get
    val changedResultsAndMarkup = ({
      for (c <- changed)
        yield {
          Some(ls.node.command_start(c), c,
              Responses.extractResults(ls, c),
              Responses.extractMarkup(ls, c))
        }
      }).flatten

    asyncExec {
      import org.eclipse.jface.text.Position
      var toDelete : Seq[(Command, Seq[Annotation])] = Seq()
      var annotationsToAdd : Seq[(Command, Seq[(Annotation, Position)])] = Seq()
      var errorsToAdd : Seq[(Command, (Int, Int), String)] = Seq()
      var errorsToDelete : Seq[Command] = Seq()

      val am =
        if (CoqoonUIPreferences.ProcessingAnnotations.get) {
          getAnnotationModel
        } else None
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
            import dk.itu.coqoon.ui.utilities.EclipseConsole
            var commandHasErrors = false
            for ((_, tree) <- results) {
              (getFile, Responses.extractError(tree), posMap) match {
                case (Some(f), Some((id, msg, Some(start), Some(end))),
                      Some(map)) =>
                  val r = Region(start - 1, length = (end - start))
                  val errors =
                    for ((region, po) <- map;
                         f <- region.intersection(r);
                         nr = f.translate(po - region.start))
                      yield (command, (nr.start, nr.end), msg)
                  if (!errors.isEmpty) {
                    errorsToAdd ++= errors
                  } else EclipseConsole.err.println(
                      s"""Error for unmapped command "${command.source}", """ +
                      s"characters ${r.start} to ${r.end}: ${msg}")
                  commandHasErrors = true
                case (Some(f), Some((id, msg, _, _)), Some(map)) =>
                  val errors =
                    for ((region, po) <- map;
                         nr = region.move(po))
                      yield (command, (nr.start, nr.end), msg)
                  if (!errors.isEmpty) {
                    errorsToAdd ++= errors
                  } else EclipseConsole.err.println(
                      s"""Error for unmapped command "${command.source}": """ +
                      msg)
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

        if (!del.isEmpty || !add.isEmpty)
          am.foreach(_.replaceAnnotations(del.toArray, add))

        if (!toDelete.isEmpty || !errorsToAdd.isEmpty ||
            !errorsToDelete.isEmpty)
          getFile.foreach(file =>
            new dk.itu.coqoon.ui.pide.UpdateErrorsJob(file,
                ls.node.commands,
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
        import dk.itu.turbocharger.java.{JavaDefinitions,
          DecoratedJavaDocument, DecoratedJavaCoqDocument}
        val doc = new DecoratedJavaDocument(f.getFile, p.getTokens)
        val pdoc =
          try {
            DecoratedJavaCoqDocument.generateCompletePIDEDocument(doc)
          } catch {
            case p : JavaDefinitions.UnsupportedException =>
              /* FIXME: We do actually want error reporting, even at this early
               * point */
              Seq()
          }
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

  protected[ui] var partitioner : Option[TokeniserPartitioner] = None

  import dk.itu.turbocharger.coq.PIDEDiff
  private[ui] lazy val differ = new PIDEDiff

  private object DecoratedJavaDocumentSetupParticipant
      extends IDocumentSetupParticipant {
    import org.eclipse.jface.text.IDocument
    override def setup(doc : IDocument) =
      partitioner = Some(DecoratedJavaPartitions.installPartitioner(
          doc, DecoratedJavaPartitions.ID))
  }

  override protected def initializeEditor() = {
    setDocumentProvider(new ForwardingDocumentProvider(
      DecoratedJavaPartitions.ID, DecoratedJavaDocumentSetupParticipant,
      new TextFileDocumentProvider {
        override def getDefaultEncoding() = "UTF-8"
      }))
    setSourceViewerConfiguration(createSourceViewerConfiguration)
    super.initializeEditor
  }

  override def findCommand(offset : Int) : Option[(Int, Command)] = {
    for ((docOffset, (command, regions)) <- pideDocument) {
      for ((r, po) <- regions;
           hr = r.move(po).extend(1) if hr.contains(offset))
        return commands.find(_._1 == docOffset)
    }
    None
  }
}
