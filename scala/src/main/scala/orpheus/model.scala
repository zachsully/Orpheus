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
import com.cra.figaro.library.atomic.continuous.dirichlet
import orpheus.data.music._

// since all data is categorical, all of these will have a dirichlet prior over
// their children
class MeasPitchclass {
}

class MeasAccidental {
}

class MeasDuration {
}

class MeasPrimitive {
}

class MeasMusic {
}
