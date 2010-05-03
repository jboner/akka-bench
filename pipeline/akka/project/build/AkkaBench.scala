import sbt._

class AkkaBench(info: ProjectInfo) extends DefaultProject(info) {
 
  // repos
  val akka = "akka maven repository" at "http://scalablesolutions.se/akka/repository"
  val databinder = "DataBinder" at "http://databinder.net/repo"
  val configgy = "Configgy" at "http://www.lag.net/repo"
  val codehaus = "Codehaus" at "http://repository.codehaus.org"
  val codehaus_snapshots = "Codehaus Snapshots" at "http://snapshots.repository.codehaus.org"
  val jboss = "jBoss" at "http://repository.jboss.org/maven2"
  val m2 = "m2" at "http://download.java.net/maven/2"
  val guiceyfruit = "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
  val google = "Google" at "http://google-maven-repository.googlecode.com/svn/repository"
  val java_net = "java.net" at "http://download.java.net/maven/2"
  val scala_tools_snapshots = "scala-tools snapshots" at "http://scala-tools.org/repo-snapshots"
  val scala_tools_releases = "scala-tools releases" at "http://scala-tools.org/repo-releases"

  // deps
  val akka_core = "se.scalablesolutions.akka" % "akka-core_2.8.0.Beta1" % "0.8.1" % "compile"

  override def mainClass = Some("benchmark.akka.ActorManager")
}
