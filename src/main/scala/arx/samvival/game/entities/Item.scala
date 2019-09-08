package arx.samvival.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/28/18
  * Time: 8:14 AM
  */

import arx.Prelude._
import arx.core.macros.GenerateCompanion
import arx.core.math.Sext
import arx.engine.entity.Taxon
import arx.engine.lworld.{Breakdown, LEntity}

@GenerateCompanion
class Item extends SVAuxData {
	var wornOn : Map[BodyPart, Int] = Map()
	var durability : Sext = Sext(25)
}

@GenerateCompanion
class Inventory extends SVAuxData {
	var heldItems = List[LEntity]()
	var itemCountLimit = none[Int]
}

@GenerateCompanion
class Equipment extends SVAuxData {
	var equipped : Set[LEntity] = Set()
}

@GenerateCompanion
class Weapon extends SVAuxData {
	var attacks : Map[AnyRef, LEntity] = Map()

	var accuracyBonus : Sext = 0
	var damageBonus : Map[DamageType, Sext] = Map()
	var extraDamage : Map[AnyRef, DamageElement] = Map()
	var usesBodyParts : Map[BodyPart, Int] = Map()
}

object NoWeapon extends Weapon {

}



class BodyPart(nomen_ : String, parents_ : Taxon*) extends Taxon(nomen_, parents_.toList)
object BodyPart {
	case object BaseBodyPart extends BodyPart("body part")
	case object Gripping extends BodyPart("gripping body part", BaseBodyPart)
	case object Thinking extends BodyPart("thinking body part", BaseBodyPart)
	case object Appendage extends BodyPart("appendage", BaseBodyPart)
	case object DextrousAppendage extends BodyPart("dextrous appendage", Appendage)

	case object Hand extends BodyPart("hand", Gripping)
	case object Pseudopod extends BodyPart("pseudopod", Gripping, DextrousAppendage)
	case object Arm extends BodyPart("arm", DextrousAppendage)
	case object Leg extends BodyPart("leg", Appendage)
	case object Head extends BodyPart("head", Thinking)
}
