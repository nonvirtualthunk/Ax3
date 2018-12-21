package arx.samvival.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/19/18
  * Time: 7:03 AM
  */

import arx.Prelude._
import arx.core.introspection.Field
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import arx.engine.entity.Taxon


object Taxonomy {
	val UnknownThing = Taxon("unknown thing", Nil)

	val Material = Taxon("material", Nil)

	object Materials {
		val Wood = Taxon("wood", Material :: Nil)
		val Stone = Taxon("stone", Material :: Nil)
		val Metal = Taxon("metal", Material :: Nil)
	}

	val LivingThing = Taxon("living thing", Nil)
	val Creature = Taxon("creature", LivingThing :: Nil)
	val Monster = Taxon("monster", Creature :: Nil)

	object Creatures {
		val Human = Taxon("human", Creature :: Nil)

		val MudMonster = Taxon("mud monster", Monster :: Nil)
	}

	val Item = Taxon("item", Nil)
	val Weapon = Taxon("weapon", Item :: Nil)
	val Axe = Taxon("axe", Item :: Nil)

	object Weapons {
		val BattleAxe = Taxon("battleaxe", Weapon :: Axe :: Nil)

		val Sword = Taxon("sword", Weapon :: Nil)

		val Longsword = Taxon("longsword", Weapon :: Sword :: Nil)
		val Shortsword = Taxon("shortsword", Weapon :: Sword :: Nil)
	}

	
}

@GenerateCompanion
class IdentityData extends TAuxData {
	var name : Option[String] = None
	var taxons: List[Taxon] = List()
}

object IdentityData {
	val Sentinel = new IdentityData
	val name = Field.fromValue(Sentinel.name).createField[IdentityData]("name",f => f.name, (f,name) => f.name = name)
	val taxons = Field.fromValue(Sentinel.taxons).createField[IdentityData]("taxons",f => f.taxons, (f,taxons) => f.taxons = taxons)
}