package com.random.nbt

import org.apache.ivy.Ivy
import org.apache.ivy.core.settings.IvySettings
import java.io.File
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.plugins.matcher.PatternMatcher
import org.apache.ivy.plugins.matcher.ExactPatternMatcher
import org.apache.ivy.plugins.resolver.IBiblioResolver
import org.apache.ivy.plugins.resolver.CacheResolver
import org.apache.ivy.plugins.resolver.FileSystemResolver
import org.apache.ivy.core.search.ModuleEntry
import org.apache.ivy.core.search.OrganisationEntry
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import org.apache.ivy.plugins.resolver.DependencyResolver
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.retrieve.RetrieveOptions
import com.typesafe.scalalogging.LazyLogging

object IvyHelper extends FileUtils with LazyLogging {
  val localRepo = resolveLocalRepoPath()
  val cacheResolver = createCacheResolver()
  val ibiblioResolver = createBiblioResolver()

  def resolveLocalRepoPath() = {
      val homeDir = System.getenv("HOME")
      val repoCandidate = new File(s"$homeDir/.ivy2/cache")
      if (repoCandidate.exists()) {
        repoCandidate.getAbsolutePath
      } else {
        val fallback = "/tmp"
        logger.error(s"Error: can't resolve local ivy cache, falling back to $fallback")
        fallback
      }
  }

  def createCacheResolver() = {
    val cr = new CacheResolver()
    cr.addIvyPattern(localRepo + "/[organisation]/[module]/ivy-[revision].xml")
    cr.addArtifactPattern(localRepo + "/[organisation]/[module]/[type]s/[artifact]-[revision].[ext]")
    cr
  }

  def createBiblioResolver() = {
    val br = new IBiblioResolver()
    br.setM2compatible(true)
    br.setUsepoms(true)
    br.setName("central")
    br
  }
}

class IvyHelper {
  import IvyHelper._

  private def createIvy(resolver: DependencyResolver) = {
    val settings = new IvySettings
    settings.setDefaultCache(new File(localRepo))
    settings.addResolver(resolver)
    settings.setDefaultResolver(resolver.getName())

    Ivy.newInstance(settings)
  }

  def getLocalModuleVersions(org: String, module: String) = {
    implicit val ivy = createIvy(cacheResolver)

    listSortedRevisions(org, module)
  }

  def getModuleDependenciesInfo(org: String, module: String, revision: String, dependencyConfiguration: String) = {
    val ivy = createIvy(ibiblioResolver)

    val ro = new ResolveOptions()
    // this seems to have no impact, if you resolve by module descriptor (in contrast to resolve by ModuleRevisionId)
    ro.setTransitive(true);
    // if set to false, nothing will be downloaded
    ro.setDownload(false);

    // 1st create an ivy module (this always(!) has a "default" configuration already)
    val md = DefaultModuleDescriptor.newDefaultInstance(
        ModuleRevisionId.newInstance(org, module+"-envelope", revision)
    )

    val ri = ModuleRevisionId.newInstance(org, module, revision)

    // don't go transitive here, if you want the single artifact
    val dd = new DefaultDependencyDescriptor(md, ri, false, false, true)

    // map to master to just get the code jar. See generated ivy module xmls from maven repo
    // on how configurations are mapped into ivy. Or check
    // e.g. http://lightguard-jp.blogspot.de/2009/04/ivy-configurations-when-pulling-from.html
    dd.addDependencyConfiguration("default", dependencyConfiguration)
    md.addDependency(dd)

    ivy.getResolveEngine.getDependencies(md, ro, new ResolveReport(md))

  }

  def getAvailableModuleVersions(org: String, module: String) = {
    implicit val ivy = createIvy(ibiblioResolver)

    listSortedRevisions(org, module)
  }

  def listSortedRevisions(org: String, module: String)(implicit ivy: Ivy)  = {
    val (correctedModule, revisions) = ivy.listRevisions(org, module) match {
      case revisions =>
        if (revisions.isEmpty)
          listUsingScalaMajorMinorVersion(org, module)
        else
          (module, revisions)
    }
    (correctedModule, sortVersionsDesc(revisions))
  }

  private def listUsingScalaMajorMinorVersion(org: String, module: String)(implicit ivy: Ivy) = {
    getScalaMajorMinorVersion match {
      case Some(scalaMajorMinorVersion) =>
        val augmentedModule = s"${module}_${scalaMajorMinorVersion}"
        (augmentedModule, ivy.listRevisions(org, augmentedModule))
      case None => (module, Array[String]())
    }
  }

  private def getScalaMajorMinorVersion = {
    Context.getString("scalaVersion") map { scalaVersion =>
      val parts = scalaVersion.split("\\.")
      s"${parts(0)}.${parts(1)}"
    }
  }

  def resolveModule(org: String, module: String, revision: String) = {
    val ivy = createIvy(ibiblioResolver)

    val ro = new ResolveOptions()
    // this seems to have no impact, if you resolve by module descriptor (in contrast to resolve by ModuleRevisionId)
    ro.setTransitive(true);
    // if set to false, nothing will be downloaded
    ro.setDownload(true);

    // 1st create an ivy module (this always(!) has a "default" configuration already)
    val md = DefaultModuleDescriptor.newDefaultInstance(
        ModuleRevisionId.newInstance(org, module+"-envelope", revision)
    )

    val ri = ModuleRevisionId.newInstance(org, module, revision)

    // don't go transitive here, if you want the single artifact
    val dd = new DefaultDependencyDescriptor(md, ri, false, false, false)

    // map to master to just get the code jar. See generated ivy module xmls from maven repo
    // on how configurations are mapped into ivy. Or check
    // e.g. http://lightguard-jp.blogspot.de/2009/04/ivy-configurations-when-pulling-from.html
    dd.addDependencyConfiguration("default", "master")
    md.addDependency(dd)

    ivy.resolve(md, ro)
  }

  def getModuleJarFileName(org: String, module: String, revision: String) = {
    localRepo + s"/$org/$module/jars/$module-$revision.jar"
  }

  def sortVersionsDesc(versions: Seq[String]): Seq[String] = {
    versions.sortWith { (version1, version2) =>
      val mismatches = (version1.split("\\.") zip version2.split("\\.")).dropWhile { case (left, right) => left == right }
      if (mismatches.length > 0) {
        val (left, right) = mismatches(0)
        Integer.valueOf(left) >= Integer.valueOf(right)
      } else true
    }
  }

  def getLastLocalVersion(org: String, module: String) = {
    val (correctedModule, versions) = getLocalModuleVersions(org, module)
    (correctedModule, versions.headOption)
  }

  def getLastLocalVersionIgnoreModuleName(org: String, module: String) = {
    getLastLocalVersion(org: String, module: String)._2
  }

  def getLastLocalVersionFilePath(org: String, module: String) = {
    val (correctedModule, revisions) = getLastLocalVersion(org, module)
    revisions map (getModuleJarFileName(org, correctedModule, _))
  }
}
