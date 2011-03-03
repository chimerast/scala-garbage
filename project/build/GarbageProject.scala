import sbt._

class ScalaTanDashProject(info: ProjectInfo) extends DefaultProject(info) {
  override def fork = Some(new ForkScalaRun {
    override def runJVMOptions =
      super.runJVMOptions ++ Seq("-server")
    override def scalaJars =
      buildScalaInstance.libraryJar :: buildScalaInstance.compilerJar :: Nil
  })
}
