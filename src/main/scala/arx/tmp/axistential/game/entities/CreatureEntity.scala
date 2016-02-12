package arx.axistential.game.entities

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 2:43 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.TAIAgent
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.effects.Dead
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.data.entity.animal.AnimalData
import arx.axistential.game.data.entity.animal.EatingData
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.representation.ConfigValue
import arx.core.traits.TSentinel
import arx.core.units.UnitOfDistance
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.TSanityCheckable.Assert
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData
import arx.tmp.game.logic.entities.traits.TArchetypedEntity
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import arx.macros.NetworkedAuxData

import scala.language.implicitConversions

class CreatureEntity extends TPhysicalEntity with TAIAgent with TArchetypedEntity {
	def species = _archetype.expectAs[AnimalSpecies].getOrElse(AnimalSpecies.Sentinel)
	def species_= (s : AnimalSpecies) { _archetype = s }

	aux[CreatureStateData]

	override protected def createPhysData(): PhysicalEntityData = {
		val PD = super.createPhysData ()
		PD.dynamic = true
		PD.motive = true
		PD.collisionShape = new CubeCollisionShape(0.5.voxel x 0.5.voxel x 4.0.voxels)
		PD.materialComposition = List( MaterialComposition( Material.withName("flesh") , 1.0f ) )
		PD
	}

	override def archetype: GameArchetype = _archetype
	override def archetype_= (a : GameArchetype) : Unit = a match {
		case sp : AnimalSpecies => _archetype = sp
		case _ => Noto.warn("Cannot give a creature entity a non-AnimalSpecies archetype")
	}

	def die () { Dead.applyToCreature(this) }

	/* +====================+ Sanity Checks +====================+ */
	override def sanityChecks: List[Assert] =
		Assert(hasAuxData[EatingData],"Creature has no eating data") ::
		Assert(hasAuxData[AnimalData],"Creature has no animal data") ::
		Assert(hasAuxData[CreaturePropertiesData],"Creature has no creature properties data") ::
		super.sanityChecks
}

/** Data about a creature's capabilities, senses, movement speed, possible acceleration, etc. Expected to
  * be present on the species (representing the average/basepoint capabilities of that species) and on an
  * individual (representing the actual current values for that creature) */ 
@NetworkedAuxData
class CreaturePropertiesData extends TNetworkedGameEntityAuxData with TConfigurableGameEntityAuxData with TInheritableAuxData {
	var sightRange = Moddable(15.meters)
	var hearingRange = Moddable(20.meters)
	/** 0.0 blind, 1.0 average sight, 2.0 would increase detection rate by 2x over average */
	var sightPrecision = Moddable(1.0f)
	var hearingPrecision = Moddable(1.0f)

	var baseMovementSpeed = Moddable(2.m_s)
	var movementAcceleration = Moddable(0.4.m_s2)

	/** Base range [0.0,1.0] representing the chance of remaining undetected by sight from 50% distance of an
	  * average precision observer for 1 moment. */
	var baseVisualStealth = Moddable(0.08f)
	/** Same range and intent as visual counterpart, but for auditory detection */
	var baseAudioStealth = Moddable(0.08f)

	def maximumObservationRange : UnitOfDistance = max(sightRange,hearingRange)
	
	override def createFromSML(sml: ConfigValue): Option[CreaturePropertiesData] = {
		if (sml.hasField("sightRange") || sml.hasField("baseMovementSpeed")) {
			val CD = new CreaturePropertiesData
			CD.sightRange = sml.sightRange.asDistance
			CD.hearingRange = sml.hearingRange.asDistance
			CD.sightPrecision = sml.sightPrecision.floatOrElse(CD.sightPrecision)
			CD.hearingPrecision = sml.hearingPrecision.floatOrElse(CD.hearingPrecision)

			CD.baseMovementSpeed = sml.baseMovementSpeed.asSpeed
			CD.movementAcceleration = sml.movementAcceleration.accelerationOrElse(CD.movementAcceleration)

			CD.baseVisualStealth = sml.baseVisualStealth.floatOrElse(CD.baseVisualStealth)
			CD.baseAudioStealth = sml.baseVisualStealth.floatOrElse(CD.baseAudioStealth)

			Some(CD)
		} else { None }
	}
}


/** Data about the current state of a creature */
@NetworkedAuxData
class CreatureStateData extends TNetworkedGameEntityAuxData {
	/** Do not modify directly, use the Dead status effect instead */
	var _alive = Moddable(true)
	def alive = _alive

	/** How fast this creature is currently moving, if applicable */
	var currentMovementSpeed = Moddable(0.0.m_s)
	/** The gait the creature is currently moving with, used to indicate how fast it's moving relative to it's ability,
	  * as opposed to in absolute distance */
	var currentMovementGait = Moddable(MoveGait.Walk)

	/** Whether or not this entity is currently attempting to remain undetected */
	var stealthActive = Moddable(false)

	/** Used to represent unusual situations that alter movement rate (as opposed to intrinsic speed), caught in a trap
	  * or entangled in some way, for example could use this to reduce movement */
	var movementRateModifier = Moddable(1.0f)
}


object CreatureEntity {
	val Sentinel : CreatureEntity = new CreatureEntity with TSentinel {}
	implicit def toCreatureData(ce : CreatureEntity) : CreaturePropertiesData = ce.aux[CreaturePropertiesData]
	implicit def toCreatureStateData(ce : CreatureEntity) : CreatureStateData = ce.aux[CreatureStateData]
}