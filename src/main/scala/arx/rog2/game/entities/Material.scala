package arx.rog2.game.entities

/**
  * TODO: Add javadoc
  */

import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TSentinel
import arx.core.vec.Vec4f
import arx.engine.entity.GameArchetype
import arx.engine.entity.TArchetypeKind
import arx.graphics.helpers.Color
import arx.graphics.helpers.HSBA

class Material(nomen : String) extends GameArchetype(nomen, Material) {

	var color = HSBA(0.0f,0.0f,0.5f,1.0f)
	var texture =  MaterialTexture.Plain
	var solid = true
	var opacity = 1.0f

}


object Material extends TArchetypeKind {
	val Sentinel = new Material("Sentinel") with TSentinel {
		opacity = 0.0f
	}



	val Wood = new Material("Wood")
	Wood.color = Color.RGBAtoHSBA(Color(97,37,6,255))
	Wood.texture = MaterialTexture.CleanLined

	val DarkWood = new Material("DarkWood")
	DarkWood.color = Color.RGBAtoHSBA(Color(130,60,20,255))
	DarkWood.texture = MaterialTexture.CleanLined

	val Stone = new Material("Stone")
	Stone.color = Color.RGBAtoHSBA(Color(80,80,90,255))
	Stone.texture = MaterialTexture.Noisy

	val FlagStones = new Material("FlagStones")
	FlagStones.color = Color.RGBAtoHSBA(Color(100,100,110,255))
	FlagStones.texture = MaterialTexture.Bordered

	val Grass = new Material("Grass")
	Grass.color = Color.RGBAtoHSBA(Color(60,130,18,255))
	Grass.texture = MaterialTexture.Noisy
}


class MaterialTexture(nomen : String) extends ArxEnum(nomen)
object MaterialTexture extends ArxEnumObject[MaterialTexture] {
	val Plain = MaterialTexture("plain")
	val Noisy = MaterialTexture("noisy")
	val Bordered = MaterialTexture("bordered")
	val Lined = MaterialTexture("lined")
	val CleanLined = MaterialTexture("clean_lined")
}