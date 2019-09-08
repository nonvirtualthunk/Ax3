package arx.samvival.game.logic

import arx.core.math.Sext
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.data.Reduceable
import arx.engine.lworld.LWorld
import arx.samvival.game.actions.AttackAction
import arx.samvival.game.entities.CharacterClass.Spearman
import arx.samvival.game.entities._
import arx.samvival.game.entities.DamageType.Piercing
import arx.samvival.game.entities.Fields._
import arx.samvival.game.entities.Species.Human
import org.scalatest.FlatSpec
import arx.samvival.game.entities.DicePoolBuilder._
import arx.samvival.game.entities.Taxonomy.AttackTypes

class Testbed {
	implicit val world = new LWorld
	implicit val view = world.view

	world.register[CombatData]
	world.register[CharacterInfo]
	world.register[Levels]
	world.register[Physical]
	world.register[Terrain]
	world.register[Vegetation]

	val playerFaction = world.createEntity()
	world.attachData(playerFaction, Faction(f => {
		f.player = true
	}))

	val enemyFaction = world.createEntity()
	world.attachData(enemyFaction, Faction(f => {
		f.player = true
	}))

	val char = world.createEntity()
	world.attachData(char, CharacterInfo(c => {
		c.actionPoints = Reduceable(Sext(6))
		c.health = Reduceable(10)
		c.species = Human
		c.faction = playerFaction
	}))
	world.attachData(char, Levels(l => {
		l.classLevels += Spearman -> 1
	}))
	world.attachData(char, CombatData(c => {
		c.accuracyBonus = Sext(0)
	}))
	world.attachData(char, Physical(p => p.position = AxialVec3(0,0,0)))


	val enemy = world.createEntity()
	world.attachData(enemy, CharacterInfo(c => {
		c.faction = enemyFaction
		c.health = Reduceable(5)
		c.actionPoints = Reduceable(Sext(4))
	}))
	world.attachData(enemy, CombatData(c => {
		c.accuracyBonus = Sext(0)
	}))
	world.attachData(enemy, Physical(p => p.position = AxialVec3(1,0,0)))
}

class AttacksTest extends FlatSpec {
	import arx.core.introspection.FieldOperations._


	"Attacks" should "be able to calculate the prospect of an attack from one character to another" in {
		val testbed = new Testbed
		import testbed._

		val attack = world.createEntity()
		world.attachData(attack).ofType[AttackData](a => {
			a.strikeAPCost = Sext(2)
			a.damage = Map("base" -> DamageElement(1 d 6, Piercing))
			a.accuracyBonus = 1
		}).ofType[IdentityData](i => {
			i.name = "slash"
			i.taxons = Set(AttackTypes.SlashAttack)
		})


		val prospects = Attacks.attackProspect(AttackAction(char, AxialVec3(0,0,0), Set(enemy), None, attack))
		assert(prospects.nonEmpty)
		assert(prospects.head.strikes.nonEmpty)

		println(prospects)
	}

	"tmp" should "test" in {

	}
}
