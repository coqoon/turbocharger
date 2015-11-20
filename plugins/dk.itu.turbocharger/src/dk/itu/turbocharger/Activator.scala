package dk.itu.turbocharger

import org.osgi.framework.BundleContext
import org.eclipse.ui.plugin.AbstractUIPlugin

class Activator extends AbstractUIPlugin {
  override def start(context : BundleContext) = {
    super.start(context)
    Activator.instance = this
  }
  
  override def stop(context : BundleContext) = {
    try {
      Activator.instance = null
    } finally {
      super.stop(context)
    }
  }
}
object Activator {
  private var instance : Activator = _
  
  def getDefault() = instance
}

object ManifestIdentifiers {
  final val PLUGIN = "dk.itu.turbocharger"

  object Markers {
    final val JAVA_PROBLEM = "dk.itu.coqoon.turbocharger:marker:java"
  }
}