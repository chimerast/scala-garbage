import sbt._

class ScalaGarbageProject(info: ProjectInfo) extends DefaultProject(info) {
  override def fork = forkRun("-server" :: "-Xms512m" :: "-Xmx512m" :: Nil)
}
