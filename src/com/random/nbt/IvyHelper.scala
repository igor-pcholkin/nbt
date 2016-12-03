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

class IvyHelper {
  def getLocalModuleVersions(org: String, module: String) = {
    val settings = new IvySettings
    val localRepo = "/Users/igor/.ivy2/cache"

    val cr = new CacheResolver()
    cr.addIvyPattern(localRepo + "/[organisation]/[module]/ivy-[revision].xml")
    cr.addArtifactPattern(localRepo + "/[organisation]/[module]/[type]s/[artifact]-[revision].[ext]")

    settings.addResolver(cr)
    settings.setDefaultResolver(cr.getName())

    val ivy = Ivy.newInstance(settings)

    ivy.listRevisions(org, module)
  }

  def getAvailableModuleVersions(org: String, module: String) = {
    val settings = new IvySettings
    val localRepo = "/Users/igor/.ivy2/cache"

    val br = new IBiblioResolver()
    br.setM2compatible(true)
    br.setUsepoms(true)
    br.setName("central")

    settings.addResolver(br)
    settings.setDefaultResolver(br.getName())

    val ivy = Ivy.newInstance(settings)

    ivy.listRevisions(org, module)
  }

  def resolveModule(org: String, module: String, revision: String) = {
    val settings = new IvySettings
    val localRepo = "/Users/igor/.ivy2/cache"
    settings.setDefaultCache(new File(localRepo))

    val br = new IBiblioResolver()
    br.setM2compatible(true)
    br.setUsepoms(true)
    br.setName("central")

    settings.addResolver(br)
    settings.setDefaultResolver(br.getName())

    val ivy = Ivy.newInstance(settings)

    val ro = new ResolveOptions()
    // this seems to have no impact, if you resolve by module descriptor (in contrast to resolve by ModuleRevisionId)
    ro.setTransitive(true);
    // if set to false, nothing will be downloaded
    ro.setDownload(true);

    // 1st create an ivy module (this always(!) has a "default" configuration already)
    val md = DefaultModuleDescriptor.newDefaultInstance(
        // give it some related name (so it can be cached)
        ModuleRevisionId.newInstance(
            org,
            module+"-envelope",
            revision
        )
    );

    val ri = ModuleRevisionId.newInstance(
            org,
            module,
            revision
        );

    // don't go transitive here, if you want the single artifact
    val dd = new DefaultDependencyDescriptor(md, ri, false, false, false);

    // map to master to just get the code jar. See generated ivy module xmls from maven repo
    // on how configurations are mapped into ivy. Or check
    // e.g. http://lightguard-jp.blogspot.de/2009/04/ivy-configurations-when-pulling-from.html
    dd.addDependencyConfiguration("default", "master");
    md.addDependency(dd);

    ivy.resolve(md, ro)
  }

  def getModuleJarFileName(org: String, module: String, revision: String) = {
    val settings = new IvySettings
    val localRepo = "/Users/igor/.ivy2/cache"

    localRepo + s"/$org/$module/jars/$module-$revision.jar"
  }
}
