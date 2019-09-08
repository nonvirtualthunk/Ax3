package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.core.math.Sext
import arx.core.vec.coordinates._
import arx.engine.data.Reduceable
import arx.engine.entity.Taxon
import arx.engine.lworld.LEntity
import arx.graphics.helpers.HSBA

@GenerateCompanion
class CharacterInfo extends SVAuxData {
	var species : Species = Species.Human
	var faction : LEntity = LEntity.Sentinel

	var health = Reduceable(6)
	var healthRecoveryRate = 1
	var alive = true
	var actionPoints = Reduceable(Sext(6))
	var moveSpeed = Sext.ofInt(6)
	var movePoints = Reduceable(Sext.ofInt(0))

	var bodyParts = Set[BodyPart]()

	var strength : Sext = 0
	var dexterity : Sext = 0
	var intellect : Sext = 0
	var cunning : Sext = 0
}

@GenerateCompanion
class Physical extends SVAuxData {
	var position : AxialVec3 = AxialVec3.Zero
	var exactPositionOverride : Option[CartVec3] = None
	var colorMultiplier : HSBA = HSBA.White
	var facing : HexDirection = HexDirection.Top
	var occupiesHex : Boolean = true

	def exactPosition = exactPositionOverride.getOrElse(position.asCartesian)
}

@GenerateCompanion
class CombatData extends SVAuxData {
	var accuracyBonus : Sext = 0
	var damageBonuses : Map[DamageType, Sext] = Map()
	var dodgeBonus : Sext = 0
	var armorBonus : Sext = 0

	var activeAttack : Option[LEntity] = None
}