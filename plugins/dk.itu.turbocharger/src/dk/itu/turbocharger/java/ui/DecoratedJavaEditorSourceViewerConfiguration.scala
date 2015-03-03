package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.java.Partitioning

import org.eclipse.jface.text.source.{ISourceViewer, SourceViewerConfiguration}
import org.eclipse.jface.text.reconciler.MonoReconciler

class DecoratedJavaEditorSourceViewerConfiguration(
    editor : DecoratedJavaEditor) extends SourceViewerConfiguration {
  override def getConfiguredContentTypes(v : ISourceViewer) =
    Partitioning.TYPES

  private val reconciler = CacheSlot {
    val r = new MonoReconciler(
        new DecoratedJavaReconcilingStrategy(editor), true)
    r.setDelay(300)
    r.setIsAllowedToModifyDocument(false)
    r.setIsIncrementalReconciler(true)
    r
  }
  override def getReconciler(v : ISourceViewer) = reconciler.get

  override def getPresentationReconciler(v : ISourceViewer) = {
    import org.eclipse.jface.text.presentation.PresentationReconciler
    val pr = new PresentationReconciler
    pr.setDocumentPartitioning(Partitioning.ID)
    DecoratedJavaEditorSourceViewerConfiguration.addDamagerRepairers(pr)
  }
}
object DecoratedJavaEditorSourceViewerConfiguration {
  import org.eclipse.jface.text.rules.DefaultDamagerRepairer
  import org.eclipse.jface.text.presentation.PresentationReconciler

  private final val tools = CacheSlot {
    import org.eclipse.jdt.ui.PreferenceConstants
    import org.eclipse.jdt.ui.text.JavaTextTools
    new JavaTextTools(PreferenceConstants.getPreferenceStore)
  }

  def addDamagerRepairers(pr : PresentationReconciler) = {
    import dk.itu.coqoon.ui.{
      CoqTokenScanner, StringTokenScanner, CommentTokenScanner}
    import Partitioning._
    import org.eclipse.jface.text.rules.RuleBasedScanner
    for ((typ, scanner) <- Seq(
        (Coq.ContentTypes.COQ, new CoqTokenScanner),
        (Coq.ContentTypes.COMMENT, new CommentTokenScanner),
        (Coq.ContentTypes.STRING, new StringTokenScanner),
        (Java.ContentTypes.JAVA, tools.get.getCodeScanner),
        (Java.ContentTypes.CHAR, tools.get.getCodeScanner),
        (Java.ContentTypes.COMMENT, tools.get.getMultilineCommentScanner),
        (Java.ContentTypes.STRING, tools.get.getStringScanner))) {
      val r = new DefaultDamagerRepairer(scanner)
      pr.setDamager(r, typ)
      pr.setRepairer(r, typ)
    }
    pr
  }
}
