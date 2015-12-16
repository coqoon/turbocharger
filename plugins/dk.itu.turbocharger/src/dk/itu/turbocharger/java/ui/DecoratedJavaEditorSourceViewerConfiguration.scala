package dk.itu.turbocharger.java.ui

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.java.DecoratedJavaPartitions

import org.eclipse.jface.text.source.{ISourceViewer, SourceViewerConfiguration}
import org.eclipse.jface.text.reconciler.MonoReconciler

class DecoratedJavaEditorSourceViewerConfiguration(
    editor : DecoratedJavaEditor) extends SourceViewerConfiguration {
  override def getConfiguredContentTypes(v : ISourceViewer) =
    DecoratedJavaPartitions.TYPES

  override def getPresentationReconciler(v : ISourceViewer) = {
    import org.eclipse.jface.text.presentation.PresentationReconciler
    val pr = new PresentationReconciler
    pr.setDocumentPartitioning(DecoratedJavaPartitions.ID)
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
    import DecoratedJavaPartitions.Types._
    import org.eclipse.jface.text.rules.RuleBasedScanner
    for ((typ, scanner) <- Seq(
        (COQ, new CoqTokenScanner),
        (COQ_COMMENT, new CommentTokenScanner),
        (COQ_STRING, new StringTokenScanner),
        (JAVA, tools.get.getCodeScanner),
        (JAVA_CHAR, tools.get.getCodeScanner),
        (JAVA_COMMENT, tools.get.getMultilineCommentScanner),
        (JAVA_STRING, tools.get.getStringScanner))) {
      val r = new DefaultDamagerRepairer(scanner)
      pr.setDamager(r, typ)
      pr.setRepairer(r, typ)
    }
    pr
  }
}
