package com.random.nbt

import org.eclipse.core.resources.IWorkspaceRoot
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin

object EclipseManager {
  def createProject(projectName: String) = {
    val root = ResourcesPlugin.getWorkspace().getRoot();
    val project = root.getProject(projectName);
    project.create(null);
    project.open(null);
  }
}
