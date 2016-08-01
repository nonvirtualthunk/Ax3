package arx.slime.game.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class CreatureData extends TGameEntityAuxData {
	var maxHP = Moddable(1)
	var hp : Int = maxHP
	var landMovement = Moddable(0)
	var landSurvival = Moddable(0.0f)
	var waterMovement = Moddable(1)
	var waterSurvival = Moddable(1.0f)
	var strength = Moddable(1)
	var armor = Moddable(0)

	var color = Moddable(ReadVec4f(1.0f,1.0f,1.0f,1.0f))
}
