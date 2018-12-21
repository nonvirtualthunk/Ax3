package arx.quintessence.game.entities

import arx.Prelude._
import arx.core.introspection.Field
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import arx.engine.entity.Taxon


object Taxonomy {
	val UnknownThing = Taxon("unknown thing", Nil)

	val Matter = Taxon("material", Nil)



	object Materials {
		val Wood = Taxon("wood", Matter :: Nil)
		val Earth = Taxon("stone", Matter :: Nil)
		val Metal = Taxon("metal", Matter :: Nil)
		val Essence = Taxon("essence", Matter)

		object Metals {
			val Lead = Taxon("lead", Metal)
			val Iron = Taxon("iron", Metal)
			val Copper = Taxon("copper", Metal)
			val Tin = Taxon("tin", Metal)
			val Mercury = Taxon("mercury", Metal :: Essence :: Nil)
			val Silver = Taxon("silver", Metal)
			val Gold = Taxon("gold", Metal)
		}

		object Essences {
			val Mercury : Taxon = Metals.Mercury
			val Salt = Taxon("salt", Essence)
			val Sulphur = Taxon("sulphur", Essence)
		}
	}

	val Item = Taxon("item", Nil)
	val Weapon = Taxon("weapon", Item :: Nil)

	object Weapons {

	}

	val Fluid = Taxon("fluid", Matter)
	val Liquid = Taxon("liquid", Fluid)
	val Gas = Taxon("gas", Fluid)

	object Gasses {
		val Aether = Taxon("aether", Gas)
		val Air = Taxon("air", Gas)
	}

	object Liquids {
		val Water = Taxon("water", Liquid)
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