//*****************************************************************************/
//                                                                  2017.01.11
// Module      :  data.music
// Copyright   :  Copyright (c) 2017 Zach Sullivan
// License     :  BSD3
// Maintainer  :  zsulliva@cs.uoregon.edu
// Stability   :  experimental
// Portability :  scala
//
// Describing Musical Data
//
//*****************************************************************************/

sealed abstract class Pitchclass
case object A extends Pitchclass
case object B extends Pitchclass
case object C extends Pitchclass
case object D extends Pitchclass
case object E extends Pitchclass
case object F extends Pitchclass
case object G extends Pitchclass

sealed abstract class Accidental
case object Sharp   extends Accidental
case object Flat    extends Accidental
case object Natural extends Accidental

sealed abstract class Duration
case object Breve          extends Duration
case object Semibreve      extends Duration
case object Minim          extends Duration
case object Crotchet       extends Duration
case object Quaver         extends Duration
case object Semiquaver     extends Duration
case object Demisemiquaver extends Duration

sealed abstract class Note
case class NoteN ( pitch  : Pitchclass
		 , accs   : List[Accidental]
		 , octive : Int
	         , durN   : Duration         ) extends Note
case class RestN ( durR   : Duration         ) extends Note

sealed abstract class Music
case class Prim ( prim : Note )         extends Music
case class Seq  ( seq : (Music,Music) ) extends Music
case class Par  ( par : (Music,Music) ) extends Music
