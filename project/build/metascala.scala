import sbt._

class MetascalaProject(info: ProjectInfo) extends DefaultProject(info) {
  override def mainSources =
    descendents(path("src"), "*.scala") --- ("src" / "test" ***)
  override def testSources =
    descendents("src" / "test", "*.scala")
}
