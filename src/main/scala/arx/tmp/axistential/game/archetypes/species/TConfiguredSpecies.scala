package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/15
 * Time: 9:02 AM
 */

import arx.axistential.game.archetypes.traits.TConfigurablePhysicalEntityArchetype
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.logic.physics.CollisionShape
import arx.core.representation.ConfigValue
import arx.core.units.UnitOfMeasure
import arx.core.vec.Vec3f
import arx.tmp.game.logic.entities.data.TConfigurableArchetype

trait TConfiguredSpecies extends Species with TConfigurablePhysicalEntityArchetype with TConfigurableArchetype {
	var collisionShapesByAgeCategory = Map[AgeCategory,CollisionShape]()
	
	def parseAgeCategoryStartTimes (sml : ConfigValue): Unit = {
		val matureIndex = speciesKind.matureAgeCategoryIndex
		if (sml.ageOfMaturity.nonEmpty) {
			val ageOfMaturity = UnitOfMeasure.parseUnitOfGameTime(sml.ageOfMaturity.str)
			for ((ac,i) <- speciesKind.ageCategories.zipWithIndex) {
				if (i <= matureIndex) {
					ageCategoryStartTimes += ac -> (ageOfMaturity * (i.toFloat / matureIndex.toFloat))
				} else {
					val k = (i - matureIndex).toFloat / (speciesKind.ageCategories.size - matureIndex)
					ageCategoryStartTimes += ac -> (ageOfMaturity * (1.0f + k * speciesKind.defaultAgeAfterMaturityMultiplier))
				}
			}
		}
	}

	def parseAgeCategoryCollisionShapes (sml : ConfigValue): Unit = {
		// if a base collision shape is defined
		if (sml.shape.nonEmpty || sml.dimensions.nonEmpty) {
			val matureIndex = speciesKind.matureAgeCategoryIndex
			val baseCollShape = parseCollisionShape(sml,defaultShape = "box")
			for ((ac,i) <- speciesKind.ageCategories.zipWithIndex) {
				val percentMature = (i+1).toFloat / matureIndex.toFloat
				if (i == 0) {
					defaultYoungestCollisionShape match {
						case Some(youngestShape) => collisionShapesByAgeCategory += ac -> youngestShape
						case None => collisionShapesByAgeCategory += ac -> (baseCollShape.scaledBy(Vec3f(percentMature)))
					}
				} else if (i <= matureIndex) {
					collisionShapesByAgeCategory += ac -> (baseCollShape.scaledBy(Vec3f(percentMature)))
				} else {
					collisionShapesByAgeCategory += ac -> baseCollShape
				}
			}
		}
	}
	
	def parseAgeCategoryInformation (sml : ConfigValue): Unit = {
		if (sml.ageCategories.nonEmptyValue) {
			for ((acStr,acSml) <- sml.ageCategories.fields ; ac = speciesKind.ageCategoryWithName(acStr) ) {
				if (acSml.startsAtAge.nonEmpty) {
					ageCategoryStartTimes += ac -> UnitOfMeasure.parseUnitOfGameTime(acSml.startsAtAge.str)
				}
				if (acSml.shape.nonEmpty || acSml.dimensions.nonEmpty) {
					collisionShapesByAgeCategory += ac -> parseCollisionShape(acSml,defaultShape = "box")
				}
			}
		}
	}

	def defaultYoungestCollisionShape : Option[CollisionShape] = None

	override def collisionShapeForAgeCategory(ageCategory: AgeCategory) = {
		collisionShapesByAgeCategory(ageCategory)
	}
}
