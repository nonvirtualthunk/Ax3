package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.core.math.Sext
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.entity.Taxon
import arx.engine.lworld.{Breakdown, LEntity}

@GenerateCompanion
class AttackData extends SVAuxData {
	var weapon : LEntity = LEntity.Sentinel
	var accuracyBonus : Int = 0
	var strikeAPCost : Sext = Sext(1)
	var staminaCost : Int = 0
	var minRange : Int = 0
	var maxRange : Int = 1
	var damage : Map[AnyRef, DamageElement] = Map()
}


trait AttackModifier {
	var nameOverride : Option[String] = None
	var accuracyBonus : Int = 0
	var strikeAPCostBonus : Sext = Sext(0)
	var staminaCostBonus = 0
	var minRangeOverride : Option[Int] = None
	var maxRangeOverride : Option[Int] = None
	var damageBonuses : Map[AnyRef, DamageElement] = Map()
}

class SpecialAttack extends AttackModifier

object SpecialAttack {
	case object PiercingStab extends SpecialAttack {
		nameOverride = Some("piercing stab")
		accuracyBonus = -1
	}
}

class DamageType(nomen_ : String, parents_  : List[Taxon]) extends Taxon(nomen_, parents_)

object DamageType extends DamageType("damage type", Nil) {
	case object Physical extends DamageType("physical", DamageType :: Nil)
	case object Bludgeoning extends DamageType("bludgeoning", Physical :: Nil)
	case object Piercing extends DamageType("piercing", Physical :: Nil)
	case object Slashing extends DamageType("slashing", Physical :: Nil)
}

case class DamageElement (damageDice : DicePool, damageType : DamageType)
object DamageElement {
	def toString(elements : Traversable[DamageElement]) = {
		elements.map(e => e.damageDice.toString)
	}
}


case class AttackProspect(attacker : LEntity, defender : LEntity, strikes : List[StrikeProspect])
case class StrikeProspect(accuracy : Breakdown[Sext],
								  dodge: Breakdown[Sext],
								  damageBonuses: Breakdown[Map[DamageType,Sext]],
								  armor : Breakdown[Sext],
								  damageElements : Breakdown[List[DamageElement]])

case class AttackResult(attacker : LEntity, defender : LEntity, strikes : List[StrikeResult])
case class StrikeResult (outcomes : Set[StrikeOutcome], damage : List[DamageResult])
case class DamageResult(amount : Int, damageType : DamageType)

trait StrikeOutcome
object StrikeOutcome {
	case object Miss extends StrikeOutcome
	case object Hit extends StrikeOutcome
	case object Dodged extends StrikeOutcome
	case object Armored extends StrikeOutcome
	case object Blocked extends StrikeOutcome
}


trait TargetPattern {
	def targetedHexes (sourcePoint : AxialVec3, targetPoint : AxialVec3) : Seq[AxialVec3]
}
object TargetPattern {
	case object Point extends TargetPattern {
		override def targetedHexes(sourcePoint: AxialVec3, targetPoint: AxialVec3): Seq[AxialVec3] = targetPoint :: Nil
	}
	case class Line(length : Int) extends TargetPattern {
		override def targetedHexes(sourcePoint: AxialVec3, targetPoint: AxialVec3): Seq[AxialVec3] = {
			val sourceCart = sourcePoint.qr.asCartesian
			val delta = targetPoint.qr.asCartesian - sourceCart
			(0 until length).map(i => AxialVec.fromCartesian(sourceCart + delta * i.toFloat)).map(ax => AxialVec3(ax.q, ax.r, sourcePoint.l))
		}
	}
}