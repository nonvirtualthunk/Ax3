package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.core.vec.ReadVec4f
import arx.graphics.helpers.HSBA

@GenerateCompanion
class Faction extends SVAuxData {
	var color : HSBA = HSBA.White

	var player : Boolean = true
}


object FactionColors {
	val Colors = List(HSBA.fromRGBA(0.8f,0.2f,0.1f,1.0f), HSBA.fromRGBA(0.2f,0.8f,0.1f,1.0f), HSBA.fromRGBA(0.1f,0.2f,0.8f,1.0f), HSBA.fromRGBA(0.3f,0.4f,0.8f,1.0f))
}