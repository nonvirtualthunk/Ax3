package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/15
 * Time: 9:02 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.entity.SpeciesKind
import arx.axistential.game.logic.physics.CollisionShape
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.entities.core.GameArchetype

abstract class Species extends GameArchetype with TPhysicalEntityArchetype {
	 def speciesKind : SpeciesKind

	 var products : List[LivingThingProduct] = Nil
	 var ageCategoryStartTimes = Map[AgeCategory,UnitOfTime]()

	 def ageOfMaturity = ageCategoryStartTimes.getOrElse(speciesKind.matureAgeCategory,{Noto.warn(s"Age of maturity requested for ${this.name}, but none specified");1.sowing})
	 def collisionShapeForAgeCategory (ageCategory : AgeCategory) : CollisionShape
 }
