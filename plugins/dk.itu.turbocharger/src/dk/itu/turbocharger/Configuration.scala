/* Configuration.scala
 * Glue to add Coq builder support to Java projects
 * Copyright © 2015, 2016 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.turbocharger

import dk.itu.coqoon.core.model.ICoqProject
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.ISources
import org.eclipse.core.runtime.{Path, Status, IProgressMonitor}
import org.eclipse.core.commands.{ExecutionEvent, AbstractHandler}
import org.eclipse.core.resources.{
  ICommand, IProject, WorkspaceJob, IProjectDescription}
import org.eclipse.core.expressions.IEvaluationContext
import org.eclipse.jface.viewers.IStructuredSelection

class ConfigureHandler extends AbstractHandler {
  import SharedConfigurationUtilities._

  override def execute(ev : ExecutionEvent) = {
    getSelection(ev.getApplicationContext).flatMap(TryCast[IProject]).foreach {
      case p =>
        new UpdateProjectDescription(
            p, ICoqProject.configureDescription(p.getDescription)).schedule
    }
    null
  }
}

class UnconfigureHandler extends AbstractHandler {
  import SharedConfigurationUtilities._
  override def execute(ev : ExecutionEvent) = {
    getSelection(ev.getApplicationContext).flatMap(TryCast[IProject]).foreach {
      case p =>
        new UpdateProjectDescription(
            p, ICoqProject.deconfigureDescription(p.getDescription)).schedule
    }
    null
  }
}

private class UpdateProjectDescription(
    project : IProject, description : IProjectDescription)
        extends WorkspaceJob(s"Reconfiguring project ${project.getName}") {
  setRule(project.getWorkspace.getRoot)

  override def runInWorkspace(monitor : IProgressMonitor) = {
    project.setDescription(description, monitor)
    Status.OK_STATUS
  }
}

private object SharedConfigurationUtilities {
  def getSelection(ec : Object) : Seq[Any] = {
    TryCast[IEvaluationContext](ec) match {
      case Some(e) =>
        TryCast[IStructuredSelection](
            e.getVariable(ISources.ACTIVE_CURRENT_SELECTION_NAME)) match {
          case Some(s) =>
            import scala.collection.JavaConversions.asScalaBuffer
            s.toList
          case None =>
            Seq()
        }
      case _ =>
        Seq()
    }
  }
}