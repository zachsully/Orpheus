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
// import com.cra.figaro.library.atomic.continuous.dirichlet
import orpheus.data.music._

// Naively --
// since all data is categorical, all of these will have a dirichlet prior over
// their children

// Learning this model from data will capture the most probable musical
// structures




// On second thought, the most important aspect we want to capture if we are
// learning from data is how the key center, rhythm, melody, tempo, and loudness
// change over time.
// Because of this, a FOCUS on a markov model will be preferable. That is, we
// make the Seq constructor the first thing we consider
