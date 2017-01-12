//*****************************************************************************/
//                                                                  2017.01.11
// Copyright   :  Copyright (c) 2017 Zach Sullivan
// License     :  BSD3
// Maintainer  :  zsulliva@cs.uoregon.edu
//
//*****************************************************************************/

import sbt._
import Keys._

class OrpheusBuild extends Build {
  override val settings = super.settings ++ Seq(
    organization := "com.orpheus"
  )
}
