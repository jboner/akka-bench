import sbt._

class AkkaBench(info: ProjectInfo) extends DefaultProject(info) {
  override def mainClass = Some("benchmark.Chameneos")
}
