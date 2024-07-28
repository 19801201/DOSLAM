name := "spinal_slam_dev"

version := "0.1"

scalaVersion := "2.11.12"

fork := true

retrieveManaged := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.9.3",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.9.3",
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % "1.9.3"),
)

//lazy val root = (project in file("."))
//  .dependsOn(ProjectRef(file("C:/myData/ev/spinalHDL/SpinalHDL"), "spinalhdl"))