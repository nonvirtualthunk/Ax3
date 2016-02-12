package arx.axistential.game.logic.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/13
 * Time: 9:40 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.lighting.computors.FancyLocalLightComputor
import arx.axistential.game.logic.lighting.computors.OptimizedGlobalLightingComputor
import arx.axistential.game.logic.lighting.computors.TGlobalLightComputor
import arx.axistential.game.logic.lighting.computors.TLocalLightComputor
import arx.core.datastructures.SynchronizedQueue
import arx.core.metrics.Metrics
import arx.core.query.ContinuousQuery
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures._
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.lighting.AddNewLightSourceTask
import arx.tmp.game.logic.lighting.BulkUpdateLightingTask
import arx.tmp.game.logic.lighting.RemoveLightSourceTask
import arx.tmp.game.logic.world.data.LightData

import scala.collection.mutable

/**
  * Created by IntelliJ IDEA.
  * User: nvt
  * Date: 11/11/11
  * Time: 10:27 PM
  * Created by nonvirtualthunk
  */

class LightingComponent extends GameEngineComponent with ContinuousQueryListener[TLightSource] {
	 var globalComputor : TGlobalLightComputor = new OptimizedGlobalLightingComputor(() => terrainGrid)
//	var globalComputor : TGlobalLightComputor = new AnyAxisLightComputor
	 var localComputors = List[TLocalLightComputor](FancyLocalLightComputor)//DirectionalLocalLightComputor,BasicLocalLightComputor)
	 var useUnsafeParallelization = false
	 var lightSourceEntities : ContinuousQuery[TLightSource] = null
	 val initializedLightSources = new mutable.WeakHashMap[TLightSource,Boolean]
	val workQueue = new SynchronizedQueue[Any]

	 def withGlobalComputor ( gc : TGlobalLightComputor ) = { this.globalComputor = gc; this }

	 expectedInitializationTime = Some(6.seconds)
	 percentInitialized = () => Some(initializedCount / toInitializeCount)

	 var toInitializeCount = 1.0f
	 var initializedCount = 0.0f

	def terrainGrid = world.aux[TerrainData].materialGrid

	 override def initialize() {
		 Noto.debug("Starting to initialize lighting for environment")
		 val terrain = terrainGrid
		 terrain.onEvent{
			 case TaleaModificationsCompletedEvent(taleae) => {
//				 worker.tell(BulkUpdateLightingTask(taleae.toList), Actor.noSender)
				 workQueue.enqueue(BulkUpdateLightingTask(taleae.toList))
			 }
		 }

		 Noto.debug("Initialize lighting event connections established")
		 val lightInitTime = Timed {
			 //We want to log changes so we can do incremental updates based on the changes made.
			 terrain.enableLogging()
			 Noto.debug("Enabled logging on blocks in environment")

			 def quadrant (v : Vec3i) = (if(v.x < env.center.x){0}else{1}) + (if(v.y< env.center.y){0}else{2})
			 val axes = Array((Vec3i(1,0,0),Vec3i(0,1,0)) , (Vec3i(-1,0,0),Vec3i(0,1,0)) ,(Vec3i(-1,0,0),Vec3i(0,-1,0)) , (Vec3i(1,0,0),Vec3i(0,-1,0)))
			 val origins = (for ( (x,y) <- axes ) yield {
				 Vec3i(if ( x.x < 0 ) { env.worldRegionAsSpatialRegion.maxX + Talea.dimension } else { env.worldRegionAsSpatialRegion.minX - Talea.dimension },
						 if ( y.y < 0 ) { env.worldRegionAsSpatialRegion.maxY + Talea.dimension } else {env.worldRegionAsSpatialRegion.minY - Talea.dimension } ,0)
			 }).toArray
			 for ( (x,y) <- axes ) { x.x *= Talea.dimension; y.y *= Talea.dimension }

			 var edgeTaleae : List[VoxelCoord] = Nil
			 var taleaeToSer : Set[VoxelCoord] = Set()
			 var taleaeToPar : List[List[VoxelCoord]] = Nil
			 val setupTime = Timed {
				 taleaeToSer = (env.aux[LightData].globalLighting(0).allTaleaPositionsInRegionInclusive(env.worldRegionAsSpatialRegion)).toSet
				 taleaeToPar = (for(i <- 0 until 4 ) yield {
					 var res = List[VoxelCoord]()
					 var x = origins(i).x; while ( (x < env.center.x && origins(i).x < env.center.x) || (x > env.center.x && origins(i).x > env.center.x) ) {
						 var y = origins(i).y; while ( (y < env.center.y && origins(i).y < env.center.y) || (y > env.center.y && origins(i).y > env.center.y) ) {

							 if ( x < env.worldRegionAsSpatialRegion.minX || x > env.worldRegionAsSpatialRegion.maxX || y < env.worldRegionAsSpatialRegion.minY || y > env.worldRegionAsSpatialRegion.maxY ) {
								 val stack = (for ( z <- (env.worldRegionAsSpatialRegion.maxZ+Talea.dimension * 3) to (env.worldRegionAsSpatialRegion.minZ-Talea.dimension) by -Talea.dimension ) yield {
									 VoxelCoord(x,y,z)
								 }).toList
								 edgeTaleae :::= stack
							 } else {
								 val stack = (for ( z <- env.worldRegionAsSpatialRegion.maxZ to env.worldRegionAsSpatialRegion.minZ by -Talea.dimension ) yield {
									 VoxelCoord(x,y,z)
								 }).toList
								 val baseQuadrant = quadrant(Vec3i(x,y,0))
								 val farQuadrant = quadrant(Vec3i(x,y,0) + axes(i)._1 + axes(i)._2)

								 if ( baseQuadrant != farQuadrant ) {
								 } else {
									 res :::= stack
									 taleaeToSer = taleaeToSer -- stack
								 }
							 }
						 y += axes(i)._2.y}
					 x += axes(i)._1.x}
					 res
				 }).toList
			 }
			 Noto.debug("Lighting Component, work dividing time : " + setupTime)
			 Noto.debug("Lighting Component, Parallelizable taleae : " + taleaeToPar.flatten.size)
			 Noto.debug("Lighting Component, Serial taleae : " + taleaeToSer.size)

			 if ( useUnsafeParallelization ) { Noto.info("Using unsafe parallelization, light results not guaranteed to be correct") }
			 val parList = if ( useUnsafeParallelization ) { (taleaeToSer.toList :: taleaeToPar) } else { taleaeToPar }
			 val serSet = if ( useUnsafeParallelization ) { Nil } else { taleaeToSer }

			 lightSourceEntities = env.createEntityTypeQuery[TLightSource].withListener(this,fireOnExistingResults = false)

			 toInitializeCount = parList.map(_.size).sum.toFloat + serSet.size.toFloat + (edgeTaleae.size.toFloat * 0.1f) + lightSourceEntities.results.size * 5.0f
			 initializedCount = 0.0f


			 for ( (primaryQ,gli) <- List(Bottom -> 0) ) {
				 globalComputor.preInitializeLighting(env,primaryQ,gli)
				 edgeTaleae.foreach { p => globalComputor.preInitializeLightingForTalea(env,p,primaryQ,gli); initializedCount += 0.1f }
				 parList.par.foreach { posList =>
					 posList.foreach { p => {
						 globalComputor.preInitializeLightingForTalea(env,p,primaryQ,gli)
						 initializedCount += 0.1f
					 } }
				 }
				 serSet.foreach { p => { globalComputor.preInitializeLightingForTalea(env,p,primaryQ,gli); initializedCount += 0.1f } }

				 parList.par.foreach { posList =>
					 posList.foreach {  p => {
						 globalComputor.initializeLightingForTalea(env,p,primaryQ,gli)
						 initializedCount += 0.9f
					 } }
				 }
				 serSet.foreach { p => globalComputor.initializeLightingForTalea(env,p,primaryQ,gli);initializedCount += 0.9f }

				 lightSourceEntities.results.foreach( _.lightChannelIndex = -1 )
				 lightSourceEntities.results.par.foreach( lightSource => { addLightSource(lightSource) ; initializedCount += 5.0f } )
			 }
		 }

		 LightingComponent.lightingInitGauge.set(lightInitTime.deltaSeconds.toFloat)
	 }

	 override def exit () {
//		 system.shutdown()
//		 system.awaitTermination()
	 }

	//	val system = ActorSystem("LightingActorSystem")
	lazy val worker = if (!gameEngine.serialMode) {
		new LightingThreadWorker(world, globalComputor, localComputors, workQueue).andStart()
	} else {
		new LightingThreadWorker(world, globalComputor, localComputors, workQueue)
	}
//	 var worker = new WorkerActor
//	 worker.start()

	 def update(f: UnitOfTime): Unit = {
		 if (gameEngine.serialMode) {
			 while (!workQueue.isEmpty) {
				 worker.whileRunningDo()
			 }
		 }
	 }

	 def createNewLocalLightGrid(t:TLightSource) = {
		 val g = new GenericTaleaGrid[Byte,LightData.LightTaleaType](0.toByte,(v:VoxelCoord) => new LightData.LightTaleaType (v,0.toByte ))
		 g.userData = Some(t)
		 g
	 }

	 def addLightSource(t : TLightSource, serial : Boolean = true) {
		 if ( ! initializedLightSources.contains(t) ) {
			 synchronized {
				 initializedLightSources.put(t,true)

				 val lightData = env.auxData[LightData]
				 val requiresNewChannel = t.motile || t.directional

				 val channelIndex = if ( requiresNewChannel ) {
					 lightData.addLocalLightChannel(createNewLocalLightGrid(t),t.lightColor)
				 } else {
					 lightSourceEntities.results.find{ case src => src.getClass.equals(t.getClass) && src.lightChannelIndex != -1 } match {
						 case Some(src) => src.lightChannelIndex
						 case None => {
							 lightData.addLocalLightChannel(createNewLocalLightGrid(t),t.lightColor)
						 }
					 }
				 }
				 t.lightChannelIndex = channelIndex
				 lightData.localLightChannel(t.lightChannelIndex).lightSources ::= t
			 }

			 if ( serial ) {
				 worker.subWorker.processWork(AddNewLightSourceTask(t))
			 } else {
				 workQueue.enqueue(AddNewLightSourceTask(t))
			 }
		 }
	 }
	 def queryResultAdded(t: TLightSource) {
		 Noto.info("Light source added")
		 addLightSource(t)
	 }
	 def queryResultRemoved(ls: TLightSource) {
		 val lightData = env.auxData[LightData]
		 if ( ls.motile || ls.directional ) {
			 lightData.removeLocalLightChannel(ls.lightChannelIndex)
			 ls.lightChannelIndex = -1
		 } else {
			 val lightChannel = lightData.localLightChannel(ls.lightChannelIndex)
			 lightChannel.lightSources = lightChannel.lightSources without ls

 //			if ( lightChannel.lightSources.nonEmpty ) {
				 workQueue.enqueue(RemoveLightSourceTask(ls,ls.lightLocation))
 //			} else {
 //				lightData.removeLocalLightChannel(ls.lightChannelIndex)
 //			}
		 }
	 }
 }

object LightingComponent {
	val timer = Metrics.timer("")
	val lightingInitGauge = Metrics.stateGauge("lighting init (seconds)",0.0f)
}