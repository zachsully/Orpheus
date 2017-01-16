//*****************************************************************************/
//                                                                  2017.01.11
// Copyright   :  Copyright (c) 2017 Zach Sullivan
// License     :  BSD3
// Maintainer  :  zsulliva@cs.uoregon.edu
//
//*****************************************************************************/

lazy val root = (project in file("."))
.settings(
  // name := "orpheus",
  // organization := "com.zachsully",
  // libraryDependencies += "com.cra.figaro" % "figaro_2.11" % "4.0.0.0"
)
.dependsOn(orpheus)
.aggregate(orpheus)

lazy val orpheus = Project("orpheus", file("scala"))
.settings(
  libraryDependencies += "com.cra.figaro" % "figaro_2.11" % "4.0.0.0",
  libraryDependencies += "de.sciss" %% "scalacollider" % "1.22.3"
)
