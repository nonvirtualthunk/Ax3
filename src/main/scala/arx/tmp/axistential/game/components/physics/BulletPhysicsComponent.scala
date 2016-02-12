package arx.axistential.game.components.physics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/18/13
 * Time: 3:57 PM
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.Prelude._
import arx.axistential.game.data.world.PhysicalEntityLocationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.networking.communicators.PhysicsNetworkingCommunicator
import arx.axistential.graphics.shader.GameUIShader
import arx.core.query.ContinuousQueryListener
import arx.core.traits.TArxTraversable
import arx.core.units.NoSpeed
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures._
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.networking.data.NetworkingWorldData
import com.carrotsearch.hppc.LongLongOpenHashMap
import com.carrotsearch.hppc.LongObjectOpenHashMap

import scala.language.implicitConversions
import scalaxy.loops._

//import jinngine.collision.SAP2
//import jinngine.physics.solver.NonsmoothNonlinearConjugateGradient
//import jinngine.physics.{Body, Scene, DefaultDeactivationPolicy, DefaultScene}
import arx.axistential.game.components.physics.BulletPhysicsComponent._
import arx.axistential.game.entities.traits.TPhysicalEntity

class BulletPhysicsComponent extends GameEngineComponent with ContinuousQueryListener[TPhysicalEntity] {
//	var scene : Scene = new DefaultScene(new SAP2, new NonsmoothNonlinearConjugateGradient (44), new DefaultDeactivationPolicy)
//	scene.setTimestep (0.1)
	canFoldUpdates = false

	lazy val physicalEntities = world.createEntityTypeQuery[TPhysicalEntity].withListener(this,fireOnExistingResults = true)
	lazy val TD = world.aux[TerrainData]
	lazy val PELD = world.aux[PhysicalEntityLocationData]

	var physWorld : btDynamicsWorld = null
	val blockObjects = new LongObjectOpenHashMap[btCollisionObject]()
	var blockShape : btCollisionShape = null

	var broadphase : btBroadphaseInterface = null
	var collisionConfiguration : btCollisionConfiguration = null
	var dispatcher : btCollisionDispatcher = null
	var solver : btConstraintSolver = null

	var nativeAllocs = Set[BulletBase]()

	protected val lastUpdatedAt = new LongLongOpenHashMap(20)

	def disposeOf(bs : BulletBase *): Unit = {
		for (b <- bs) {
			if (b != null) { b.dispose() }
		}
	}



	override def exit(): Unit = {
		physicalEntities.foreach(pe => queryResultRemoved(pe))
		nativeAllocs.foreach(n => n.dispose())
		for (b <- blockObjects.values) {
			if (b != null) {
				physWorld.removeCollisionObject(b)
				disposeOf(b)
			}
		}
		val arr = physWorld.getCollisionObjectArray
		var toRemove = List[btCollisionObject]()
		for (i <- 0 until arr.size optimized) {
			toRemove ::= arr.at(i)
		}
		for (r <- toRemove) {
			physWorld.removeCollisionObject(r)
			r.dispose()
		}

		disposeOf(blockShape, dispatcher, physWorld /*,collisionConfiguration*/, broadphase)
	}

	def taleaModified(base : ITalea[_]) {
		base match {
			case talea : TLoggingTalea[Byte] => {
				val taleaHash = VoxelCoord.hashL( talea.x, talea.y, talea.z )
				val lastUpdated = if ( lastUpdatedAt.containsKey( taleaHash ) ) {
					lastUpdatedAt.lget()
				} else {
					-1L
				}
				lastUpdatedAt.put(taleaHash,talea.modifiedCount)
				val mods = talea.loggedModifications.takeWhile( m => m.revision > lastUpdated )
				for ( mod <- mods ) {
					val modHash = VoxelCoord.hashL(mod.x + talea.x,mod.y + talea.y,mod.z + talea.z)
					if ( mod.oldValue > 0 && mod.newValue <= 0 ) { //from solid terrain to non solid
						if ( blockObjects.containsKey( modHash ) ) {
							val obj = blockObjects.lget()
							blockObjects.remove(modHash)

							physWorld.removeCollisionObject(obj)
							obj.dispose()
						}
					} else if ( mod.oldValue <= 0 && mod.newValue > 0 ) { //from non solid to solid
						//do nothing, this will be taken care of normally (probably)
						val modPosV = VoxelCoord(mod.x + talea.x,mod.y + talea.y,mod.z + talea.z)

						unembed(modPosV)
					}
				}
			}
			case _ =>
		}
	}

	/** Find entities near where a voxel has become solid and adjust their position as necessary
	  * to ensure that they are not embedded in the terrain
	  */
	def unembed (v : VoxelCoord) {
		for (ent <- physicalEntities.filter(pe => pe.position.scalarDistanceTo(v) < 10.0f && pe.hasAuxData[btPhysicsEntityData])) {
			while (isEmbedded(ent)) {
				PELD.entityLocationOctree.remove(ent.position,ent)
				ent.position = ent.position.plusZ(1.0f)
				PELD.entityLocationOctree.set(ent.position,ent)
				ent.aux[btPhysicsEntityData].rigidBody.translate(Vec3f(0.0f,0.0f,1.0f))
			}
		}
	}

	/** Checks whether a given physical entity is intersecting solid voxels at any point */
	def isEmbedded (ent : TPhysicalEntity) : Boolean = {
		val dims = ent.boundingDimensions.inVoxels * Vec3f(0.0f,0.0f,0.49f)
		val min = (ent.position - dims).toVoxelCoord
		val max = (ent.position + dims).toVoxelCoord
		for ( x <- min.x to max.x optimized ; y <- min.y to max.y optimized ; z <- min.z to max.z optimized ) {
			if ( TD.isSolid(x,y,z) && ! TD.isEntityAt(VoxelCoord(x,y,z)) ) { return true }
		}
		false
	}

	override def initialize(): Unit = {
		GdxNativesLoader.load()
		Bullet.init()

		broadphase = new btDbvtBroadphase()
		collisionConfiguration = new btDefaultCollisionConfiguration()
		dispatcher = new btCollisionDispatcher(collisionConfiguration)
		solver = new btSequentialImpulseConstraintSolver()

		physWorld = new btDiscreteDynamicsWorld(dispatcher,broadphase,solver,collisionConfiguration)
		physWorld.setGravity(new Vector3(0.0f,0.0f,-10.0f.meters.inVoxels))

		val callback = new InternalTickCallback(physWorld,true) {

			override def onInternalTick(dynamicsWorld: btDynamicsWorld, timeStep: Float): Unit = {
				onPhysicsTick(timeStep)
			}
		}
		callback.attach(physWorld,true)

		blockShape = new btBoxShape(new Vector3(0.5f,0.5f,0.5f))
		blockShape.setMargin(0.0f)

		world.aux[TerrainData].materialGrid.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				taleae.foreach( t => taleaModified(t) )
			}
		}

		// Trigger the lazy
		physicalEntities.size

		if (gameEngine.isServer) {
			world.aux[NetworkingWorldData].customCommunicators += new PhysicsNetworkingCommunicator(physicalEntities,world)
		}
	}


	def onPhysicsTick ( timeStep : Float ) {

//		physWorld.clearForces()
		val TD = world.aux[TerrainData]
		for ( ent <- physicalEntities ) {
//			if (ent.name.contains("wolf")) {
//				val DD = world.aux[DebugData]
//				val wp = DD.pointGroups.getOrElse("wolfpos",DebugData.PointGroup(Vec4f(1.0f,0.0f,0.0f,1.0f),true,Nil))
//				DD.pointGroups += "wolfpos" -> wp.copy(points = ent.position.plusZ(0.5f) :: wp.points)
//			}

			if (ent.collisionShapeChanged) {
				updateCollisionShape(ent)
			}

			if ( ent.dynamic && ! ent.ghost ) {
				val hdim = Vec3i((ent.boundingDimensions.inVoxels*0.5f) + 2)
				val min = ent.position.toVoxelCoord - hdim
				val max = ent.position.toVoxelCoord + hdim

				for ( x <- min.x to max.x optimized ; y <- min.y to max.y optimized ; z <- min.z to max.z optimized ) {
					val hash = VoxelCoord.hashL(x,y,z)
					if ( ! blockObjects.containsKey(hash) ) {
						if ( TD.materialGrid(x,y,z) > 0 ) {
							val block = new btCollisionObject()
							val transform = new Matrix4().idt().translate( VoxelCoord(x,y,z).toObjectCoord )
							block.setWorldTransform(transform)
							block.setCollisionShape(blockShape)
							physWorld.addCollisionObject(block,CollisionFlags.CF_STATIC_OBJECT.toShort,((1<<6) | CollisionFlags.CF_CHARACTER_OBJECT).toShort)
							blockObjects.put(hash,block)
						}
					}
				}

				val PD = ent.aux[btPhysicsEntityData]
	//			while ( TD.isSolid(ent.footVoxelPosition) ) {
	////				PD.rigidBody.getW
	////				PD.rigidBody.setLinearVelocity(Vec3f(0.0f,0.0f,20.0f))
	//			}

				if ( ent.motive ) {

					val cur = PD.rigidBody.getLinearVelocity
					val v = Vec3f(	ent.motiveVector.x.getOrElse(cur.x.v_s).inVoxelsPerSecond,
										ent.motiveVector.y.getOrElse(cur.y.v_s).inVoxelsPerSecond,
										ent.motiveVector.z.getOrElse(cur.z.v_s).inVoxelsPerSecond)

					val delta = v - cur
	//				if ( ! ent.ignoringGravity ) {

	//					PD.rigidBody.applyGravity()

	//				}


	//				PD.rigidBody.setLinearVelocity(v * 0.1f)
	//				PD.rigidBody.setLinearVelocity(new Vector3(v.x,v.y,v.z))

					PD.rigidBody.setLinearVelocity(v)

	//				PD.rigidBody.setLinearVelocity(new Vector3(0.0f,3.0f,0.0f))
	//				PD.rigidBody.setLinearVelocity(new Vector3(-0.53640294f,1.2225348E-5f,-1.3619661E-4f))
	//				println(v)

					if ( ! ent.motiveVector.isEmpty ) {
						ent.motiveVector.x = zero_ms//NoVelocity
						ent.motiveVector.y = zero_ms
						ent.motiveVector.z = NoSpeed
					}


					if (v.xy.lengthSafe > 0.01f) {
						val oldFacing = ent.facing
						ent.facing = ReadVec3f(v.x,v.y,0.0f).normalize

//						if (oldFacing.dot(ent.facing) < 0.0f && ent.name.contains("wolf")) {
//							Noto.warn(s"Switched facing from $oldFacing to ${ent.facing}")
//						}
					}
				} else {
					PD.rigidBody.applyGravity()
				}

				val oldPos = ent.position
				val newPos = ObjectCoord(PD.rigidBody.getCenterOfMassPosition)
//				val delta = newPos - oldPos
//				if (delta.xy.lengthSafe > 0.01f) {
//					ent.facing = ReadVec3f(delta.x,delta.y,0.0f).normalize
//				}

				val rawVel = PD.rigidBody.getLinearVelocity

				PELD.entityLocationOctree.remove(oldPos,ent)
				ent.position = newPos
				ent.velocity.x = rawVel.x.v_s
				ent.velocity.y = rawVel.y.v_s
				ent.velocity.z = rawVel.z.v_s
				PELD.entityLocationOctree.set(newPos,ent)
			}
		}
	}

	def update(time: UnitOfTime): Unit = {


		physWorld.stepSimulation(time.inSeconds)
	}
	
	protected lazy val defaultMargin = {
		val tmp = new btCapsuleShape(0.25f,1.0f)
		val ret = tmp.getMargin
		tmp.dispose()
		ret
	} 

	def createCollisionShapeForEnt (t : TPhysicalEntity) = {
		val dims = t.boundingDimensions.inVoxels
		val ret = if ( ! t.motive ) {
			new btBoxShape(new Vector3(dims.x*0.5f,dims.y*0.5f,dims.z*0.5f))
		} else {
			val rad = dims.x * 0.5f//sqrtf(powf(dims.x * 0.5f,2.0f) + powf(dims.y * 0.5f,2.0f))
			new btCapsuleShapeZ(rad - defaultMargin,dims.z - rad * 2.0f)
		}

		ret
	}

	def updateCollisionShape(ent: TPhysicalEntity) = {
		if (! ent.ghost) {
			if (ent.dynamic) {
				val PD = ent.aux[btPhysicsEntityData]
				posit(PD.rigidBody != null,"null rigid body on entity marked for collision shape update")
				physWorld.removeRigidBody(PD.rigidBody)
				val oldShape = PD.rigidBody.getCollisionShape
				val newShape = createCollisionShapeForEnt(ent)
				PD.rigidBody.setCollisionShape(newShape)
				oldShape.dispose()
				nativeAllocs -= oldShape
				val invMass = PD.rigidBody.getInvMass
				if (invMass > 0.0f) {
					val result = new Vector3()
					newShape.calculateLocalInertia(1.0f / invMass,result)
					PD.rigidBody.setMassProps(1.0f / invMass,result)
				}
				physWorld.addRigidBody(PD.rigidBody)
			} else {
				setVoxelsAsOccupiedByEntity(ent)
			}
		}
		// mark as no longer dirty, we have accounted for the changes
		ent.collisionShapeChanged = false
	}


	def queryResultAdded(t: TPhysicalEntity): Unit = {
		PELD.entityLocationOctree.set(t.position,t)

		if ( ! t.ghost ) {
			if ( t.dynamic ) {
				val shape = createCollisionShapeForEnt(t)
				nativeAllocs += shape
				val ms = new btDefaultMotionState(new Matrix4().idt().translate(t.position.x,t.position.y,t.position.z + 0.04f))
				val ci = new btRigidBodyConstructionInfo(1.0f,ms,shape)
				val body = new btRigidBody(ci)
				ci.dispose()
				nativeAllocs += ms
				body.setAngularFactor(new Vector3(0.0f,0.0f,0.0f))
	//			if ( t.motive ) {
					body.setActivationState(CollisionConstants.DISABLE_DEACTIVATION)
	//			}
	
				if ( t.motive ) {
					physWorld.addRigidBody(body,CollisionFlags.CF_CHARACTER_OBJECT.toShort,(CollisionFlags.CF_STATIC_OBJECT | (1 << 6)).toShort)
				} else {
					physWorld.addRigidBody(body,(1 << 6).toShort,(CollisionFlags.CF_STATIC_OBJECT | (1 << 6) | CollisionFlags.CF_CHARACTER_OBJECT).toShort)
				}
	
				val bPD = t.aux[btPhysicsEntityData]
				bPD.rigidBody = body
				bPD.motionState = ms
	
				// we can mark the collision shape as no longer being dirty
				t.collisionShapeChanged = false
			} else {
				setVoxelsAsOccupiedByEntity(t)
			}
		}
	}
	
	def setVoxelsAsOccupiedByEntity(t : TPhysicalEntity): Unit = {
		val dim = t.boundingDimensions.inVoxels
		
		if ( (dim.x > 1.0f || dim.y > 1.0f || dim.z > 1.0f) ) {
			val TD = world.aux[TerrainData]
			val opos = t.position
			val hdim = dim * 0.5f
			val min = (opos - hdim).toVoxelCoord
			val max = (opos + hdim).toVoxelCoord

			TD.materialGrid.modificationBlock(VoxelRegion.fromCorners(min,max)) {
				for ( x <- min.x to max.x optimized ; y <- min.y to max.y optimized ; z <- min.z to max.z optimized ) {
					TD.setEntityAt(x,y,z,t,TerrainByteUtils.PartiallyTransparentByte)
				}
			}
		}
	}

	def queryResultRemoved(t: TPhysicalEntity): Unit = {
		PELD.entityLocationOctree.remove(t.position,t)
		if (! t.ghost) {
			if ( t.dynamic ) {
				val PD = t.aux[btPhysicsEntityData]
				if ( PD.rigidBody != null ) {
					physWorld.removeRigidBody(PD.rigidBody)
					PD.rigidBody.dispose()
					PD.motionState.dispose()
				}
			}
		}
	}
}

object BulletPhysicsComponent {
	implicit def toV3 ( v : ReadVec3f ) : Vector3 = new Vector3(v.x,v.y,v.z)
	implicit def fromV3 ( v : Vector3 ) : ReadVec3f = ReadVec3f(v.x,v.y,v.z)
}

// TODO: how do we want to save/restore rigid bodies? Probably shouldn't persist anything here, rather store it elsewhere
// and let the physics component convert it to bt form on initialize
@SerialVersionUID(1L)
class btPhysicsEntityData extends TGameEntityAuxData with Externalizable {
	var rigidBody : btRigidBody = null
	var motionState : btMotionState = null

	def writeExternal(p1: ObjectOutput): Unit = ???
	def readExternal(p1: ObjectInput): Unit = ???
}

class BulletPhysicsDebugGraphicsComponent extends ShinDynamicGraphicsComponent {
	def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
		bucket.drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal
		val tc = bucket.textureBlock( image("test/debug_border.png") )
		for ( c <- gameEngine.components.ofType[BulletPhysicsComponent] ) {
			val numCollisionObjects = c.physWorld.getNumCollisionObjects
			val collisionObjectsArray = c.physWorld.getCollisionObjectArray
			for ( i <- 0 until numCollisionObjects optimized ) {
				val obj = collisionObjectsArray.at(i)
				if ( obj != null ) {
					val mat = new Matrix4().idt()
					val minV = new Vector3
					val maxV = new Vector3
					val shape = obj.getCollisionShape
					if ( shape != null ) {
						obj.getCollisionShape.getAabb(obj.getWorldTransform,minV,maxV)
					}
					val min : ReadVec3f = minV
					val max : ReadVec3f = maxV

					CommonRendering.drawCube(bucket,ObjectCoord((min + max)*0.5f),max - min + 0.25f,Vec4f(1.0f,1.0f,1.0f,0.3f),tc)
				}
			}
		}
	}

	override def bucketIdentifier: Symbol = 'debug

	def bucketRequirements: RenderBucketRequirements = RenderBucketRequirements(UIAttributeProfile,GameUIShader(world,graphicsEngine))
}

class VoxelDebugGraphicsComponent extends ShinDynamicGraphicsComponent {
	def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
		val tc = bucket.textureBlock( image("test/debug_border.png") )

		for (group <- world.aux[DebugData].voxelGroups.values) {
			val color = group.color
			val voxels = group.voxels.take(1000)
			val grid = new HashVoxelByteStore()
			voxels.foreach(v => grid(v) = 1.toByte)

			CommonRendering.drawVoxels(bucket,voxels : TArxTraversable[VoxelCoord],Some(grid),color,tc,1,exposedOnly = true,1.1f)
		}

		for (group <- world.aux[DebugData].pointGroups.values) {
			val color = group.color
			val points = group.points
			if (group.connected && points.size >= 2) {
				for (window <- points.sliding(2)) {
					val start = window.head
					val end = window.last
					val mid = (start + end) / 2.0f
					val forward = (end - start).normalizeSafe
					val ortho = Vec3f.UnitZ.cross(forward)
					val l = (end - start).lengthSafe

					CommonRendering.quad(bucket,start,forward,ortho,Vec2f(l,0.1f),color,tc)
				}
			} else {
				for (p <- points) {
					CommonRendering.billboardQuad(bucket,p,Vec2f(0.25f,0.25f),color,tc)
				}
			}
		}
	}

	override def bucketIdentifier: Symbol = 'debug

	def bucketRequirements: RenderBucketRequirements = RenderBucketRequirements(UIAttributeProfile,GameUIShader(world,graphicsEngine))
}
class DebugData extends TWorldAuxData {
	var active = false
	var voxelGroups = Map[String,DebugData.VoxelGroup]()
	var pointGroups = Map[String,DebugData.PointGroup]()
	var graphingData = List[(Float,Float)]()
}


object DebugData {
	var worldRef = World.Sentinel

	case class VoxelGroup (color : ReadVec4f, voxels : Set[VoxelCoord])
	case class PointGroup (color : ReadVec4f, connected : Boolean, points : List[ObjectCoord])
}


object TmpLauncher {
	implicit def toV3 ( v : ReadVec3f ) : Vector3 = new Vector3(v.x,v.y,v.z)

	def main ( args : Array[String] ) {
		GdxNativesLoader.load()
		Bullet.init(true)

		val broadphase = new btDbvtBroadphase()
		val collisionConfiguration = new btDefaultCollisionConfiguration()
		val dispatcher = new btCollisionDispatcher(collisionConfiguration)
		val solver = new btSequentialImpulseConstraintSolver()

		val world = new btDiscreteDynamicsWorld(dispatcher,broadphase,solver,collisionConfiguration)
		world.setGravity(new Vector3(0.0f,0.0f,-10.0f))

		world.stepSimulation(1.0f)

		val groundShape = new btBoxShape(Vec3f(10.0f,10.0f,1.0f))
//		val groundMS = new btDefaultMotionState(new Matrix4().idt())
//		val groundInf = new btRigidBodyConstructionInfo(0.0f,groundMS,groundShape)
//		val ground = new btRigidBody(groundInf)
//		ground.setCollisionFlags( btCollisionObject.CollisionFlags.CF_STATIC_OBJECT )
		val ground = new btCollisionObject()
		ground.setCollisionShape(groundShape)


		val ballShape = new btSphereShape(1.0f)
		val ballMS = new btDefaultMotionState(new Matrix4().idt.translate(0.0f,0.0f,4.0f))
		val ballInf = new btRigidBodyConstructionInfo(1.0f,ballMS,ballShape)
		val ball = new btRigidBody(ballInf)

		world.addCollisionObject(ground)
		world.addRigidBody(ball)


		for ( i <- 0 until 100 ) {
			world.stepSimulation(0.0166667f)
			println( ball.getCenterOfMassPosition )
		}
	}
}
