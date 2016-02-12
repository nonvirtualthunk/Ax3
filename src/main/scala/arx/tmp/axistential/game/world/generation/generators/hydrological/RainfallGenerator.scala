package arx.axistential.game.world.generation.generators.hydrological

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/14/13
 * Time: 11:19 AM
 */

import arx.Prelude._
import arx.axistential.game.data.world.SoilData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.THasSortKey
import arx.core.vec.ReadVec2i
import arx.engine.world.World
import arx.tmp.game.logic.ai.GeneralizedSearcher

import scalaxy.loops._

class RainfallGenerator extends TWorldGenerator {


	/**
	 * Perform this component's generation, storing the results in the provided world.
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if ( phase == WorldGenerator.Hydrological ) {
			if ( stage == WorldGenerator.Heightmap ) {
				val reg = world.worldRegionAsSpatialRegion
				val heightmap = world.aux[TopologicalData].heightmap
				val depthmap = world.aux[TopologicalData].depthmap
				val rainfallGrid = world.aux[SoilData].rainfall

				val cardinals = Cardinals.expandedCardinals.filter( v => v.z == 0 )
				val opposites = cardinals.map( v => cardinals.indexWhere( o => o.x == -v.x && o.y == -v.y ) )

				val results = GeneralizedSearcher.search( new GeneralizedSearcher.SearchQuery[ReadVec2i](
					reg.center.xy : ReadVec2i,
					3000,
					(v,g) => g,
					(v) => reg.contains(v) && heightmap(v) > depthmap(v),
					(v) => ! reg.contains(v),
					(v,buffer) => {
						for ( q <- 0 until cardinals.length optimized ) { buffer.append(v + cardinals(q).xy) }
					},
					(from,to) => {
						val cost = (heightmap(to) - heightmap(from)).max(1) * distance(to,from)
						cost
//						1
					}
				))


				for ( (v,n) <- results ) {
					rainfallGrid(v) = (30 * (n.g / math.max(distance(v,reg.center.xy),1)) ).toByte
				}

//				val Q = new FibonacciHeap[RainfallNode]
//				val closed = new VoxelCoordSet2D
//
//				for ( x <- reg.minX until reg.maxX optimized ) {
//					Q.enqueue(RainfallNode(x,reg.minY,0,0,600,2))
//				}
//
//				val basePercent = 0.1f
//
//
//				while ( Q.nonEmpty ) {
//					val head = Q.dequeue()
//					closed.add(head.x,head.y)
//
//					val h = heightmap((head.x>>2)<<2,(head.y>>2)<<2)
//					val d = depthmap((head.x>>2)<<2,(head.y>>2)<<2)
//					val doSuccessors = true
//
//					val z = if ( h > d ) { h } else { 0 }
//
//					val deltaRain = if ( z - head.lastZ > 3 ) {
//						(z - head.lastZ).toFloat
//					} else { 0.0f }
//
//					val thisMoisture = head.rain * (basePercent + (deltaRain/100.0f))
//
//					if ( h > d ) {
//						val currentMoisture = rainfallGrid(head.x,head.y)
//						if ( thisMoisture > currentMoisture ) {
//							rainfallGrid(head.x,head.y) = thisMoisture.toShort
//						} else { false }
//					}
//
//					if ( doSuccessors ) {
//						rainfallGrid(head.x,head.y) = thisMoisture.toShort
//
//						for ( q <- 0 until cardinals.length optimized ) {
//							if ( q != opposites(head.direction) ) {
//								val c = cardinals(q)
//
//								val ax = head.x + c.x
//								val ay = head.y + c.y
//
//								if ( ! closed.contains(ax,ay) && reg.contains(ax,ay,VoxelCoord.Center.z) ) {
//									val deltaG = if ( q == head.direction ) { 1f }
//													 else { 1.5f }
//
//									Q.enqueue(RainfallNode(ax,ay,z,head.g + deltaG,head.rain - deltaRain - (deltaG - 0.97f) * 10.0f,q))
//								}
//							}
//						}
//					} else {
//						//no need to do anything, someone else has already provided more rain here
//					}
//				}
			}
		}
		Nil
	}
}
object RainfallGenerator {
	case class RainSource ( var rain : Float )
	case class RainfallNode ( x:Int,y:Int,lastZ:Int, g : Float , rain : Float, direction : Int ) extends THasSortKey {
		def sortKey: Float = -rain
	}
}
