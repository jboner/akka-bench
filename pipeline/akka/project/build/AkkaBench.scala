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

  // deps
  val akka_core = "se.scalablesolutions.akka" % "akka-core_2.8.0.RC3" % "0.9" % "compile"

  override def mainClass = Some("benchmark.akka.ActorManager")
}
