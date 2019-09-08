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

	val Item = Taxon("item")
	val Weapon = Taxon("weapon", Item)
	val Axe = Taxon("axe", Item)

	object Weapons {
		val BattleAxe = Taxon("battleaxe", Weapon, Axe)

		val Sword = Taxon("sword", Weapon)

		val Longsword = Taxon("longsword", Weapon, Sword)
		val Shortsword = Taxon("shortsword", Weapon, Sword)
	}

	val AttackType = Taxon("attack type")
	object AttackTypes {
		val PhysicalAttack = Taxon("physical attack", AttackType)
		val SlashAttack = Taxon("slash attack", PhysicalAttack)
		val StabbingAttack = Taxon("stabbing attack", PhysicalAttack)

		val NaturalAttack = Taxon("natural attack", AttackType)

		val MeleeAttack = Taxon("melee attack", AttackType)
		val RangedAttack = Taxon("ranged attack", AttackType)
		val ReachAttack = Taxon("reach attack", MeleeAttack)


	}

	val Terrain = Taxon("terrain")

	object Terrains {
		val Flatland = Taxon("flatland", Terrain)
		val Hills = Taxon("hills", Terrain)
		val Mountains = Taxon("mountains", Terrain)
	}

	val Vegetation = Taxon("vegetation")

	object Vegetations {
		val Grassland = Taxon("grassland", Vegetation)
		val Forest = Taxon("forest", Vegetation)
		val DeciduousForest = Taxon("deciduous forest", Forest)
		val EvergreenForest = Taxon("evergreen forest", Forest)
	}

	val SpeciesRoot = Taxon("species")

	object Species {
		val Humanoid = Taxon("humanoid", SpeciesRoot)
		val Monstrous = Taxon("monstrous", SpeciesRoot)
	}

	val Skill = Taxon("skill")

	object Skills {
		val CombatSkill = Taxon("combat skill", Skill)
		val WeaponSkill = Taxon("weapon skill", CombatSkill)
		val MagicSkill = Taxon("magic skill", Skill)
		val CraftingSkill = Taxon("crafting skill", Skill)
		val MovementSkill = Taxon("movement skill", Skill)
		val SurvivalSkill = Taxon("survival skill", Skill)
		val GatheringSkill = Taxon("gathering skill", Skill)
	}

	val CharacterClass = Taxon("character class")
	object CharacterClasses {
		val CombatClass = Taxon("combat class", CharacterClass)
		val MeleeCombatClass = Taxon("melee combat class", CombatClass)
		val RangedCombatClass = Taxon("ranged combat class", CombatClass)
		val MagicClass = Taxon("magic class", CharacterClass)


	}


	val Action = Taxon("action")
	object Actions {
		val MoveAction = Taxon("move", Action)
		val AttackAction = Taxon("attack", Action)
		val GatherAction = Taxon("gather", Action)
		val SwitchActiveCharacterAction = Taxon("switch active", Action)
	}

}

@GenerateCompanion
class IdentityData extends SVAuxData {
	def this(nomen : String, taxons_ : Taxon*) {
		this()
		name = Some(nomen)
		taxons = taxons_.toSet
	}
	var name : Option[String] = None
	def name_=(s : String): Unit = {
		name = Some(s)
	}
	var taxons: Set[Taxon] = Set()

	def isA(taxon : Taxon) = taxons.exists(t => t.isA(taxon))
}