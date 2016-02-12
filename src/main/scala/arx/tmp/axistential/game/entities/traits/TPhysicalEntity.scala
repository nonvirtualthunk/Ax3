package arx.axistential.game.entities.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.logic.physics.CollisionShape
import arx.core.Moddable
import arx.core.mat.Basis
import arx.core.mat.TFrameOfReference
import arx.core.representation.InformationLevel.InformationLevel
import arx.core.units._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TSanityCheckable.Assert
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.logic.entities.traits.TArchetypedEntity
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import arx.macros.NetworkedAuxData
import arx.macros.NonTriggering

import scala.language.implicitConversions

trait TPhysicalEntity extends GameEntity with TArchetypedEntity {
	protected[traits] var internData = createInternData()

	def createInternData () = {
		val pd = createPhysData()
		addAuxData(pd)
		pd
	}

	override protected def onNewAuxDataCreated(gead: TGameEntityAuxData): Unit = {
		super.onNewAuxDataCreated (gead)

		gead match {
			case ped : PhysicalEntityData => internData = ped
			case _ =>
		}
	}

	def physData = internData

	protected def createPhysData () : PhysicalEntityData = new PhysicalEntityData

	// Workaround to avoid confusion with possible implicit conversion to Moddable, which has "dynamic" defined
	def dynamic = internData.dynamic
	def dynamic_= (d : Boolean) { internData.dynamic = d }

	/* +====================+ Sanity Checks +====================+ */
	override def sanityChecks: List[Assert] = 
		Assert(internData.collisionShape.notSentinel,"No Collision Shape Provided") :: 
		Assert(internData.materialComposition.nonEmpty,"No Material Composition provided") :: super.sanityChecks
}

object TPhysicalEntity {
	val Sentinel : TPhysicalEntity = new TPhysicalEntity {
		protected def readResolve : Object = TPhysicalEntity.Sentinel
	}

	protected[entities] val Dynamic = 1 << 0
	protected[entities] val Sleeping = 1 << 1
	protected[entities] val Motive = 1 << 2
	protected[entities] val Ghost = 1 << 3
	protected[entities] val IgnoringGravity = 1 << 4
	protected[entities] val CollisionShapeChanged = 1 << 5
	
	
	implicit def toPhysicalData (ent : TPhysicalEntity) : PhysicalEntityData = ent.internData
}

@NetworkedAuxData
class PhysicalEntityData extends TGameEntityAuxData with TFrameOfReference with TNetworkedGameEntityAuxData {
	protected var physicsFlags : Int = 0
	/** Whether or not this entity responds to physical forces */
	def dynamic = (physicsFlags & TPhysicalEntity.Dynamic) != 0
	/** Whether or not this entity is sleeping (assumed to be at a physical equilibrium) */
	def sleeping = (physicsFlags & TPhysicalEntity.Sleeping) != 0
	/** Whether or not this entity exerts force itself, as opposed to simply being acted upon */
	def motive = (physicsFlags & TPhysicalEntity.Motive) != 0
	/** Whether or not this entity really exists in a physical sense, or if it is simply a stand in for a real physics object */
	def ghost = (physicsFlags & TPhysicalEntity.Ghost) != 0
	/** Whether or not this entity has changed its collision shape */
	def collisionShapeChanged = (physicsFlags & TPhysicalEntity.CollisionShapeChanged) != 0

	def ignoringGravity : Boolean = (physicsFlags & TPhysicalEntity.IgnoringGravity) != 0

	def dynamic_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.Dynamic } else { physicsFlags &= (~TPhysicalEntity.Dynamic) } }
	def sleeping_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.Sleeping } else { physicsFlags &= (~TPhysicalEntity.Sleeping) } }
	def motive_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.Motive } else { physicsFlags &= (~TPhysicalEntity.Motive) } }
	def ghost_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.Ghost } else { physicsFlags &= (~TPhysicalEntity.Ghost) } }
	def ignoringGravity_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.IgnoringGravity } else { physicsFlags &= (~TPhysicalEntity.IgnoringGravity) } }
	def collisionShapeChanged_= ( t : Boolean ) { if ( t ) { physicsFlags |= TPhysicalEntity.CollisionShapeChanged } else { physicsFlags &= (~TPhysicalEntity.CollisionShapeChanged) } }

	/** Center of volume position */
	protected[entities] var _position : ObjectCoord = ObjectCoord.Sentinel
	/** Direction this entity is facing, expressed as a coordinates system basis */
	protected[entities] var _directionBasis : Basis = Basis.Identity
	/** Shape of the space this entity occupies */
	protected[entities] var _collisionShape : CollisionShape = CollisionShape.Sentinel

	// None of these are worth triggering a full send for, so don't trigger one
	/** Most recent velocity */
	@NonTriggering var velocity : Velocity = Velocity(zero_ms,zero_ms,zero_ms)
	/** If motive, direction in which it is motive-ing (or NoVelocity, if not exerting any force) */
	@NonTriggering var motiveVector : Velocity = NoVelocity

	@NonTriggering var facing : ReadVec3f = Vec3f.UnitX

	/** What other entity is holding/containing this entity */
	var heldBy : Option[TPhysicalEntity] = None


	/*+====================+ Controlled Getters/Setters +====================+ */
	def position = _position
	def position_= ( p : ObjectCoord ) {
		if (_position != p) {
			_position = p
			collisionShape.frameOfReferenceChanged(this)
			// do not call into the standard networking hooks, position is handled separately
		}
	}

	def directionBasis = _directionBasis
	def directionBasis_= ( p : Basis ) {
		_directionBasis = p
		collisionShape.frameOfReferenceChanged(this)
		fieldModified() // call into the standard send-all-data networking hooks
	}

	def collisionShape = _collisionShape
	def collisionShape_= ( cs : CollisionShape ) {
		if (_collisionShape != cs) {
			collisionShapeChanged = true
			_collisionShape = cs
			_collisionShape.frameOfReferenceChanged(this)
			fieldModified() // call into the standard send-all-data networking hooks
		}
	}

	/** Dimensions required to fully enclose this entity */
	def boundingDimensions : Dimensions = collisionShape.boundingDimensions
	def halfHeight = boundingDimensions.z.inVoxels * 0.5f
	/** Material composition of this entity, represented as materials by percent of volume */
	var materialComposition : Moddable[List[MaterialComposition]] = Moddable(Nil)
	/** Volume of space that this entity occupies */
	def volume : UnitOfVolume = collisionShape.volume

	def primaryMaterial = materialComposition.resolve() match {
		case Nil => Material.Sentinel
		case list => list.sortBy( _.proportion * -1.0f).head.material
	}

	def density = {
		materialComposition.foldLeft(0.kg_m3){ case (accum,mc) => accum + mc.material.density * mc.proportion }
	}
	def mass : UnitOfMass = density * volume

	/*+====================+ Convenience/Fast Functions +====================+ */
	/** Position of "feet", or where the object touches the ground in its center */
	// we add in buffer of .03 because in the event that a body is resting on the ground, we don't want to give the impression
	// that it is inside the ground, which can happen with rounding errors and precision issues
	def adjustedFootPos : ObjectCoord = position.minusZ(boundingDimensions.z.inVoxels * 0.5f - 0.05f)
	def adjustedFootVoxelPos : VoxelCoord = adjustedFootPos.toVoxelCoord
	def headPosition : ObjectCoord = position.plusZ(boundingDimensions.z.inVoxels * 0.5f)

	def fastOrientedDimensions = boundingDimensions
	def fastDimensions = boundingDimensions
	def fastFootPosition = adjustedFootPos

	def setFootPosition (pos : ObjectCoord) { position = pos.plusZ(halfHeight) }

	/* +====================+ Frame Of Reference Impl +====================+ */
	def transformFromFrameOfReference(v: ReadVec3f): ReadVec3f = directionBasis.transformAndAdd(position,v)
	def transformFromFrameOfReference(v: ObjectCoord): ObjectCoord = directionBasis.transformAndAdd(position,v)
	def transformToFrameOfReference(v: ObjectCoord): ObjectCoord = directionBasis.subInvTransform(position,v)
	
	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}

class SimplePhysicalEntity extends GameEntity with TPhysicalEntity