//*****************************************************************************/
//                                                                  2017.01.13
// Module      :  orpheus.model
// Copyright   :  Copyright (c) 2017 Zach Sullivan
// License     :  BSD3
// Maintainer  :  zsulliva@cs.uoregon.edu
// Stability   :  experimental
// Portability :  scala
//
// Describing a probabilistic model for harmonic musical data
//
//*****************************************************************************/

package orpheus.model

import com.cra.figaro.language._
import orpheus.data.music._

// We want our model to represent a distibution over data.music
class MMusic {
  lazy val rat = 1 / 7
  // val keyCenter = Select(rat -> A
  // 			,rat -> B
  // 		        ,rat -> C
  // 			,rat -> D
  // 		        ,rat -> E
  // 			,rat -> F
  // 		        ,rat -> G)
}
