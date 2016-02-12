package arx.axistential.game.components.physics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/13/13
 * Time: 1:35 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.PhysicalConstants
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.units.Acceleration
import arx.core.units._
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f

@Deprecated
class PhysicsComponent extends GameEngineComponent {
	lazy val physicalEntities = world.createEntityTypeQuery[TPhysicalEntity]

//	val normalForce = Acceleration(0.m_s2,0.m_s2,0.m_s2)
	/** This is effective gravity, that being, (gravity + normal force), 0 when standing on ground */
	private[this] val gravityForce = Vec3f(0)
	private[this] val externalForce = Vec3f(0)
	private[this] val buoyancyForce = Vec3f(0)
	private[this] val dragForce = Vec3f(0)
	private[this] val motiveForce = Vec3f(0)
	private[this] val resultForce = Vec3f(0)
	private[this] val convertedVelocity = Vec3f(0)
	private[this] val basePosition = MutableVoxelCoord(0,0,0)

	@inline def resetA ( a : Acceleration ) {
		a.x = zero_ms2
		a.y = zero_ms2
		a.z = zero_ms2
	}
	def euclidLength ( v : ReadVec3f ) = v.x*v.x+v.y*v.y+v.z*v.z

	var lastUpdated = 0l
	def update(time: UnitOfTime): Unit = {
		val perTick = gameEngine.timestep
		var remainingTicks = time

		while ( remainingTicks >= perTick ) {
			remainingTicks -= perTick
			val isLast = remainingTicks < perTick
			val curTime = System.nanoTime()
	//		println(s"Since last updated ${(curTime - lastUpdated) / 1000000000.0f}s")
			lastUpdated = curTime
			val constants = world.aux[PhysicalConstants]
			val G = constants.GravitationalConstant.inVoxelsPerSecondSquared
			val dt = perTick.inSeconds

			val terrain = world.aux[TerrainData]
			for ( ent <- physicalEntities if ent.dynamic ) {
				resultForce.x = 0
				resultForce.y = 0
				resultForce.z = 0

				val footPos = ent.adjustedFootPos
				footPos.toVoxelCoord(basePosition)

				if ( ! ent.ignoringGravity ) {
					gravityForce.z = -G
				} else {
					gravityForce.z = 0.0f
				}

				convertedVelocity.x = ent.velocity.x.inVoxelsPerSecond
				convertedVelocity.y = ent.velocity.y.inVoxelsPerSecond
				convertedVelocity.z = ent.velocity.z.inVoxelsPerSecond

	//			dragForce.x = convertedVelocity.x * -0.01f
	//			dragForce.y = convertedVelocity.y * -0.01f
	//			dragForce.z = convertedVelocity.z * -0.01f

				if ( ent.motive && ent.motiveVector.notEmpty ) {
					resultForce.x = (ent.motiveVector.x.inVoxelsPerSecond - convertedVelocity.x)/dt
					resultForce.y = (ent.motiveVector.y.inVoxelsPerSecond - convertedVelocity.y)/dt
					resultForce.z = (ent.motiveVector.z.getOrElse(ent.velocity.z).inVoxelsPerSecond - convertedVelocity.z)/dt + gravityForce.z
	//				//The motive force is the adjusted, desired direction to apply force
	//				motiveForce.x = (ent.motiveVector.x.inVoxelsPerSecond - convertedVelocity.x)
	//				motiveForce.y = (ent.motiveVector.y.inVoxelsPerSecond - convertedVelocity.y)
	//				motiveForce.z = (ent.motiveVector.z.inVoxelsPerSecond - convertedVelocity.z)
	//
	//
	//				externalForce.x = dragForce.x + buoyancyForce.x + gravityForce.x
	//				externalForce.y = dragForce.y + buoyancyForce.y + gravityForce.y
	//				externalForce.z = dragForce.z + buoyancyForce.z + gravityForce.z
	//				val externalLength = externalForce.lengthSafe
	//				val motiveLength = ent.motiveVector.lengthSafe.inVoxelsPerSecond * 1.0f
	//				val toOvercomeLength = externalLength * (1.0f - math.max(0.0f,externalForce.dot(motiveForce)))
	//
	//				if ( toOvercomeLength > motiveLength ) {
	//					//This should be proportional to the dot product, I think
	//					val newLength = externalLength - motiveLength
	//					if ( newLength > 0 ) {
	//						resultForce.x = (externalForce.x / externalLength) * newLength
	//						resultForce.y = (externalForce.y / externalLength) * newLength
	//						resultForce.z = (externalForce.z / externalLength) * newLength
	//					}
	//				} else {
	//					val newLength = clamp(motiveLength - toOvercomeLength,0.0f,motiveForce.lengthSafe)
	//					if ( newLength > 0.0f ) {
	//						resultForce.x = (motiveForce.x / motiveLength) * newLength
	//						resultForce.y = (motiveForce.y / motiveLength) * newLength
	//						resultForce.z = (motiveForce.z / motiveLength) * newLength
	//					}
	//				}
				} else {
					resultForce.x = dragForce.x + buoyancyForce.x + gravityForce.x
					resultForce.y = dragForce.y + buoyancyForce.y + gravityForce.y
					resultForce.z = dragForce.z + buoyancyForce.z + gravityForce.z
				}

	//			if ( absf(resultForce.x) > 0.0f || absf(resultForce.y) > 0.0f || absf(resultForce.z) > 0.0f )
				ent.velocity.x += (resultForce.x * dt).v_s
				ent.velocity.y += (resultForce.y * dt).v_s
				ent.velocity.z += (resultForce.z * dt).v_s

//				while ( ent.collisionShape.intersect(terrain.materialGrid).nonEmpty ) {
//					ent.position = ent.position.plusZ(1.0f)
//				}

	//			val newPos = MutableObjectCoord(ent.position)
				var axis = 0; while ( axis < 3 ) {
					val delta = ent.velocity(axis).inVoxelsPerSecond * dt
					val t = ent.collisionShape.intersect(delta,axis,terrain.materialGrid)
					if ( t >= 0.0f ) {
						val et = math.max(t - 0.05f,0.0f)
						ent.velocity(axis) = zero_ms
						val effectiveDelta = delta * et
						ent.position = ent.position.plusAxis(axis,effectiveDelta)
	//					newPos(axis) += effectiveDelta
					} else {
						ent.position = ent.position.plusAxis(axis,delta)
	//					newPos(axis) += delta
					}
				axis += 1}

	//			if ( ent.collisionShape.intersect(0.00001f,2,terrain.materialGrid) > 0.0f ) {
//				if ( ent.collisionShape.intersect(terrain.materialGrid).nonEmpty ) {
//					Noto.warn("In collision after anti-collision, why?")
//					while ( ent.collisionShape.intersect(0.0f,0,terrain.materialGrid) > 0.0f ) {
//						ent.position = ent.position.plusZ(1.0f)
//					}
//				}

				while ( ent.collisionShape.intersect(terrain.materialGrid).nonEmpty ) {
					Noto.warn("In collision after anti-collision, why?")
					ent.position = ent.position.plusZ(1.0f)
				}

	//			ent.position = newPos

				if ( ent.velocity.lengthSafe.inMetersPerSecond > 0.01f ) {
					ent.facing = ent.velocity.toVec3f.normalizeSafe
				}
				if ( isLast ) {
					ent.ignoringGravity = false
					if ( ! ent.motiveVector.isEmpty ) {
						ent.motiveVector.x = zero_ms//NoVelocity
						ent.motiveVector.y = zero_ms
						ent.motiveVector.z = NoSpeed
					}
				}
			}
		}
	}
}
