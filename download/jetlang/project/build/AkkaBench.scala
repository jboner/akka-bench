import sbt._

class AkkaBench(info: ProjectInfo) extends DefaultProject(info) {
 
  val jetlang_repo = "jetlang" at "http://jetlang.googlecode.com/svn/repo/"

  // deps
  val jetlang = "org.jetlang" % "jetlang" % "0.2.0" % "compile"
 
  override def mainClass = Some("benchmark.jetlang.Main")
}
