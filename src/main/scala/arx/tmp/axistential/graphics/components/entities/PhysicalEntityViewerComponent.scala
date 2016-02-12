package arx.axistential.graphics.components.entities

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/13
 * Time: 11:25 AM
 */

import arx.Prelude._
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.graphics.components.entities.PhysicalEntityGraphicsComponent.EntityGraphicsState
import arx.axistential.graphics.components.weather.TCloudGraphicsComponent
import arx.axistential.graphics.shader.AnthologiconBillboardShader
import arx.core.THasSortKey
import arx.core.datastructures.GrowableSortableArray
import arx.core.datastructures.SynchronizedQueue
import arx.core.query.ContinuousQueryListener
import arx.core.synchronization.ReadWriteLock
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.TaleaGrid
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.events.EntityChangedEvent
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

import scala.collection.mutable

class PhysicalEntityViewerComponent extends DynamicGraphicsComponent with ContinuousQueryListener[TPhysicalEntity]{
	def env = world

	dependencies ::= classOf[TCloudGraphicsComponent]
	lazy val cloudGraphics = reify[TCloudGraphicsComponent]

	val renderer = provideInstanceOf[TGenericEntityRenderer]
	val graphicsInfoProvider = ReflectionAssistant.provideInstanceOf[TGameEntityGraphicsInfoProvider]

	val dynamicEntities = new mutable.HashSet[TPhysicalEntity]()
	val staticEntities = new mutable.HashSet[TPhysicalEntity]()

	var lastSortedAtEyePosition = ObjectCoord(-1000.0f,-1000.0f,-1000.0f)
	val entitiesSortedByDistance = new GrowableSortableArray[EntityGraphicsState]
	val entitiesSortedByDistanceLock = new ReadWriteLock

	var visibilityIncrement = 4.0f
	def nearThreshold = graphicsEngine.pov.hiResThreshold
	def calculateVisibilityThreshold = graphicsEngine.pov.viewDistance + 10.0f + visibilityIncrement

	lazy val mainShader = {
		val s = new AnthologiconBillboardShader(env,pov _)
		s.cutoffActive = false
		s
	}

	lazy val cutoffShader = {
		val s = new AnthologiconBillboardShader(env,pov _)
		s.cutoffActive = true
		s.zcutoff.set( () => pov.zCutoff + 1.0f )
		s
	}

	def shader = if ( pov.isCutoffActive ) {
		cutoffShader
	} else {
		mainShader
	}

	private val innerStaticVBO = new AVBO(BillboardAttributeProfile)
	innerStaticVBO.useIntIndices()
	val staticRenderTarget = new SlottedVBO(innerStaticVBO)
	val staticTextureBlock = new TextureBlock(2048,2048)
	staticTextureBlock.minFilter = GL11.GL_NEAREST
	staticTextureBlock.magFilter = GL11.GL_NEAREST

	var dirtiedTaleae = new mutable.HashSet[VoxelCoord]

	case class AddedOrRemovedEntity ( entity : TPhysicalEntity , added : Boolean )
	val addedOrRemovedEntities = new SynchronizedQueue[AddedOrRemovedEntity]
	val changedEntities = new SynchronizedQueue[TPhysicalEntity]


	override def initialize() {
		gameEngine.environment.createEntityTypeQuery[TPhysicalEntity].withListener(this,fireOnExistingResults = true)
		env.aux[LightData].onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				taleae.foreach(t => dirtiedTaleae.add(VoxelCoord(t.position)))
			}
		}
		gameEngine.environment.onEvent {
			case EntityChangedEvent(entity) => entity match {
				case physEnt : TPhysicalEntity => {
					changedEntities.enqueue(physEnt)
				}
			}
		}
	}

	override def draw(graphicsContext: RenderingContext) {
		shader.bind()

//		GL11.glLoadIdentity()
		graphicsContext.pov.look()


		staticTextureBlock.bind()
		cloudGraphics.cloudTexture.bind(1)

		GL.glSetState(GL11.GL_DEPTH_TEST,enable = true)
		GL.glSetState(GL11.GL_CULL_FACE,enable = false)

		staticRenderTarget synchronized {
			if ( staticRenderTarget.vbo.lastSolidifiedMarker < staticRenderTarget.vbo.lastUpdatedMarker ) {
				staticRenderTarget.vbo.solidify(GL15.GL_STATIC_DRAW)
			}
		}
		staticRenderTarget.vbo.drawElements(GL11.GL_TRIANGLES)
	}

	override def setPointOfView(pov: TCamera) {
		super.setPointOfView(pov)
	}

	protected def update(f: Float) {
		val bucket = getBucket
		val env = gameEngine.environment
		val viewThreshold = calculateVisibilityThreshold

		bucket.textureBlock.minFilter = GL11.GL_NEAREST
		bucket.textureBlock.magFilter = GL11.GL_NEAREST
		bucket.depthFunc = GL11.GL_LEQUAL

		val eye = graphicsEngine.pov.eye

		while ( addedOrRemovedEntities.nonEmpty ) {
			val addedOrRemoved = addedOrRemovedEntities.dequeue()
			if ( addedOrRemoved.added ) {
				if ( distance(addedOrRemoved.entity.position,eye) <= viewThreshold ) {
					renderStaticEntityIntoRenderTarget(addedOrRemoved.entity,highRes = true)
				}
			} else {
				staticRenderTarget.clearRangesFor(addedOrRemoved.entity)
			}
		}

		while ( changedEntities.nonEmpty ) {
			val changed = changedEntities.dequeue()
			if ( changed.inWorld && shouldDraw(changed) ) {
				renderStaticEntityIntoRenderTarget(changed,highRes = true)
			} else {
				staticRenderTarget.clearRangesFor(changed)
			}
		}


		if ( distance(eye,lastSortedAtEyePosition) > visibilityIncrement ) {
			updateVisibility(eye)
			lastSortedAtEyePosition = ObjectCoord(eye)
		}

		entitiesSortedByDistanceLock.readLock {
			var i = 0; while ( i < entitiesSortedByDistance.size ) {
				val state = entitiesSortedByDistance(i)
				if ( state.currentDistanceToEye > viewThreshold ) { i = entitiesSortedByDistance.size }
				if ( state.staticUpdatable ) {
					val newHash = renderer.renderHash(state.entity,env,staticTextureBlock)
					if ( newHash != state.renderHash ) {
						state.renderHash = newHash
						renderStaticEntityIntoRenderTarget(state.entity,highRes = true)
					}
				}
				i += 1
			}
		}

		val taleae = dirtiedTaleae
		if ( taleae.nonEmpty ) {
			dirtiedTaleae = new mutable.HashSet[VoxelCoord]
			updateFromLighting(taleae)
		}

		val euclidLimit = graphicsEngine.pov.viewDistance * graphicsEngine.pov.viewDistance + 10.0f

		for ( ent <- dynamicEntities ) {
			if ( shouldDraw(ent) ) {
				val eDist = euclidDistance(ent.position,eye)
				if ( eDist < euclidLimit ) { //add culling based on view frustrum

					renderer.renderEntity(ent,env,bucket,bucket.textureBlock,IdentityTransform,graphicsEngine)
				}
			}
		}
	}

	def shouldDraw ( ent : TPhysicalEntity ) = true/*ent.parentless && ent.heldBy.isEmpty*/

	protected def updateFromLighting ( taleae : Traversable[VoxelCoord] ) {
		val taleaeCenters = taleae.map(_ + (Talea.dimension/2)).toArray
		val halfDimSquared = (Talea.dimension/2)
		val tolerance = 15
		val euclidDistanceLimit = halfDimSquared * 3 + tolerance*tolerance + tolerance*tolerance + tolerance*tolerance
		val viewLimit = calculateVisibilityThreshold

		entitiesSortedByDistanceLock.readLock {
			var i = 0; while ( i < entitiesSortedByDistance.size ) {
				val entityState = entitiesSortedByDistance(i)
				if ( entityState.currentDistanceToEye < viewLimit ) {
					if ( ! entityState.dynamic ) {
						val vpos = entityState.entity.position.toVoxelCoord
						var found = false
						var j = 0; while ( j < taleaeCenters.length && ! found ) {
							val center = taleaeCenters(j)
							val eDist = euclidDistance(center,vpos)
							if ( eDist < euclidDistanceLimit ) {
								found = true
							}
							j += 1}

						if ( found ) {
							renderStaticEntityIntoRenderTarget(entityState.entity,highRes = true) //Hi res has no effect, at present
						}
					}
				} else { i = entitiesSortedByDistance.size }
				i += 1
			}
		}
	}

	protected def updateVisibility ( eye : Vec3f ) {
		val viewThreshold = calculateVisibilityThreshold

		entitiesSortedByDistanceLock.readLock {
			for ( state <- entitiesSortedByDistance ) {
				state.lastDistanceToEye = state.currentDistanceToEye
				state.currentDistanceToEye = distance(eye,state.entity.position)

				if ( ! state.dynamic ) {
					if ( state.lastDistanceToEye < viewThreshold && state.currentDistanceToEye >= viewThreshold ) { //Just moved out of view
						staticRenderTarget.clearRangesFor(state.entity)
					} else if ( state.lastDistanceToEye >= viewThreshold && state.currentDistanceToEye < viewThreshold ) { //Just moved in to view
						renderStaticEntityIntoRenderTarget(state.entity,highRes = state.currentDistanceToEye < nearThreshold)
					} else if ( state.lastDistanceToEye >= nearThreshold && state.currentDistanceToEye < nearThreshold ) { //Moved into close range
						renderStaticEntityIntoRenderTarget(state.entity,highRes = true)
					} else if ( state.lastDistanceToEye < nearThreshold && state.currentDistanceToEye >= nearThreshold ) { //Moved out of close range
						renderStaticEntityIntoRenderTarget(state.entity,highRes = false)
					}
				}
			}
		}

		entitiesSortedByDistanceLock.writeLock {
			entitiesSortedByDistance.sort()
		}
	}

	def bucketIdentifier = 'physicalEntities
	def bucketRequirements = RenderBucketRequirements(BillboardAttributeProfile,shader _)

	def renderStaticEntityIntoRenderTarget(ent: TPhysicalEntity,highRes : Boolean){
		val env = gameEngine.environment
		staticRenderTarget synchronized {
			staticRenderTarget.clearRangesFor(ent)
			if ( shouldDraw(ent) ) {
				staticRenderTarget.association = Some(ent)
				renderer.renderEntity(ent,env,staticRenderTarget,staticTextureBlock,IdentityTransform,graphicsEngine)
				staticRenderTarget.association = None
				staticRenderTarget.vbo.lastUpdatedMarker += 1
			}
		}
	}

	def addStaticEntity ( ent : TPhysicalEntity ) {
		staticEntities += ent

		addedOrRemovedEntities.enqueue(AddedOrRemovedEntity(ent,added = true))
	}
	def addDynamicEntity ( ent : TPhysicalEntity ) {
		dynamicEntities += ent
	}
	def removeStaticEntity ( ent : TPhysicalEntity ) {
		staticEntities -= ent

		addedOrRemovedEntities.enqueue(AddedOrRemovedEntity(ent,added = false))
	}
	def removeDynamicEntity ( ent : TPhysicalEntity ) {
		dynamicEntities -= ent
	}

	def queryResultAdded(raw_t: TPhysicalEntity) {
		val t = raw_t

		val dynamic = t.dynamic match {
			case true => addDynamicEntity(raw_t); true
			case false => addStaticEntity(raw_t); false
		}

		val gInfo = graphicsInfoProvider.rawGraphicsInfoFor(raw_t)
		val staticUpdatable = ! dynamic && (gInfo match {
			case CustomRendererGraphicsInfo(_,_,updatable) => updatable
			case StatefulGraphicsInfo(_,_,_) => true
			case _ => false
		})
		val dynamism = if ( dynamic ) { PhysicalEntityGraphicsComponent.Dynamic }
		else if ( staticUpdatable ) { PhysicalEntityGraphicsComponent.StaticUpdatable }
		else { PhysicalEntityGraphicsComponent.Static }

		val graphicsState = new EntityGraphicsState(raw_t,dynamism,-1.0f,distance(graphicsEngine.pov.eye,raw_t.position))
		entitiesSortedByDistanceLock.writeLock {
			entitiesSortedByDistance.append(graphicsState)
		}
	}
	def queryResultRemoved(raw_t: TPhysicalEntity) {
		val t = raw_t

		entitiesSortedByDistanceLock.writeLock {
			entitiesSortedByDistance.remove( entitiesSortedByDistance.indexWhere( state => state.entity eq raw_t ) )
		}

		t.dynamic match {
			case true => removeDynamicEntity(raw_t)
			case false => removeStaticEntity(raw_t)
		}
	}

	override def intersectEntities(start_oc: ObjectCoord, baseEnd_oc: ObjectCoord,filter : (GameEntity) => Boolean ) = {
		var entitiesByT = List[(Float,GameEntity)]()
		val delta = baseEnd_oc - start_oc
		val viewThreshold = calculateVisibilityThreshold

		def intersect( ent : TPhysicalEntity ) {
			val sphereIntersection = Intersection.raySphereIntersection(start_oc,baseEnd_oc,ent.position,ent.fastDimensions.max.inVoxels * 0.5f)
			if ( sphereIntersection.nonEmpty ) {
				val shape = renderer.intersectionShapeFor(ent,gameEngine.environment)
				val subIntersection = shape.intersect(start_oc,baseEnd_oc)
				if ( subIntersection.nonEmpty ) {
					entitiesByT ::= (subIntersection.head,ent)
				} else {
					None
				}
			}
		}

		entitiesSortedByDistanceLock.readLock {
			var i = 0; while ( i < entitiesSortedByDistance.size ) {
				val state = entitiesSortedByDistance(i)
				if ( state.currentDistanceToEye > viewThreshold ) { i = entitiesSortedByDistance.size }
				if ( filter(state.entity) ) {
					intersect( state.entity )
				}
				i += 1
			}
		}

		val sortedEntities = entitiesByT.sortBy(_._1)
		sortedEntities.map( t => EntityIntersectionResult(t._2,start_oc + delta * t._1) )
	}

	override def reset() {
		staticRenderTarget.clear()
		val viewThreshold = calculateVisibilityThreshold
		entitiesSortedByDistanceLock.readLock {
			var i = 0; while ( i < entitiesSortedByDistance.size ) {
				val state = entitiesSortedByDistance(i)
				if ( state.currentDistanceToEye > viewThreshold ) { i = entitiesSortedByDistance.size }
				else {
					renderStaticEntityIntoRenderTarget(state.entity,highRes = true)
				}
				i += 1
			}
		}
	}
}
object PhysicalEntityGraphicsComponent {
	val Dynamic = 0
	val Static = 2
	val StaticUpdatable = 1


	class EntityGraphicsState ( val entity : TPhysicalEntity , val dynamism : Int , var lastDistanceToEye : Float , var currentDistanceToEye : Float, var renderHash : Int = -1 ) extends THasSortKey {
		def dynamic = dynamism == Dynamic
		def staticUpdatable = dynamism == StaticUpdatable
		def sortKey = currentDistanceToEye
	}

}
