package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.engine.entity.Taxon
import arx.samvival.game.entities.Taxonomy.Skills

class Skill(name_ : String, parentTaxons : List[Taxon]) extends Taxon(name_, parentTaxons) {
	def this(name_ : String, taxon : Taxon) { this(name_, taxon :: Nil) }
}
object Skill {
	import Skills._
	case object Melee extends Skill("melee", CombatSkill)
	case object Dodge extends Skill("dodge", CombatSkill)
	case object Ranged extends Skill("ranged", CombatSkill)
	case object ForestSurvival extends Skill("forest survival", SurvivalSkill)
}


class CharacterClass(name_ : String, parentTaxons : List[Taxon]) extends Taxon(name_, parentTaxons) {
	def this(name_ : String, taxon : Taxon) { this(name_, taxon :: Nil) }
}
object CharacterClass {
	import Taxonomy.CharacterClasses._
	case object Classless extends CharacterClass("classless", Taxonomy.CharacterClass)
	case object Spearman extends CharacterClass("spearman", MeleeCombatClass)
	case object Bowman extends CharacterClass("bowman", RangedCombatClass)
}

@GenerateCompanion
class Levels extends SVAuxData {
	var skillBonuses : Map[Skill, Int] = Map()
	var skillXp : Map[Skill, Int] = Map()
	var classLevels : Map[CharacterClass, Int]= Map()
}