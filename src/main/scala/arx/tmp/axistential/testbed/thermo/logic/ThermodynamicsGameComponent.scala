package arx.axistential.testbed.thermo.logic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/21/13
 * Time: 9:12 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.GranularData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.world.generation.generators.hydrological.LoopingTaleaIterator
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.FibonacciHeap
import arx.core.THasSortKey
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.entities.core.GameEntity
import com.carrotsearch.hppc.LongOpenHashSet

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._

class ThermodynamicsGameComponent extends GameEngineComponent with ContinuousQueryListener[GameEntity] {
	import ThermodynamicsGameComponent._
	
	lazy val iter = new LoopingTaleaIterator[Short,GenericTalea[Short]](world.aux[ThermodynamicData].temperatureDelta,world.worldRegionAsSpatialRegion)
	lazy val heatSources = world.createEntityAuxDataQuery[HeatSourceData].withListener(this,fireOnExistingResults = true)

	override def initialize () {
		heatSources.size
	}
	
	var perTick = 1
	
	def update(time: UnitOfTime): Unit = {
		val TD = world.aux[TerrainData]
		val GD = world.aux[GranularData]

		TD.materialGrid.allTaleae

		var countdown = perTick
		while ( countdown > 0 ) {
			val baseV = iter.v
			
			val globalTalea =  world.aux[ThermodynamicData].temperatureDelta.taleaFor(baseV.x,baseV.y,baseV.z)
			val localTaleaea = world.aux[ThermodynamicData].heatGridsDefinedAt(baseV)
			if ( localTaleaea.nonEmpty ) {
				countdown -= 1
				for ( x <- 0 until Talea.dimension optimized ;
						y <- 0 until Talea.dimension optimized ;
						z <- 0 until Talea.dimension optimized )
				{
					var sum = 0
					for ( i <- 0 until localTaleaea.length optimized ) {
						sum += localTaleaea(i)(x,y,z)
					}
					
					val currentValue = globalTalea(x,y,z)

					if ( currentValue != sum ) {
						val newValue = if ( sum > currentValue ) {
							currentValue + ((sum - currentValue) >> 1) + 1
						} else {
							currentValue + ((sum - currentValue) >> 1)
						}

						globalTalea(x,y,z) = newValue.toShort

						if ( newValue > 0 ) {
							val cur =  GD.levelGrid(baseV.x + x,baseV.y + y,baseV.z + z)
							if ( cur > 0 ) {
								GD.levelGrid.modificationBlock( VoxelCoord(baseV.x + x,baseV.y + y,baseV.z + z) ) {
									GD.levelGrid(baseV.x + x,baseV.y + y,baseV.z + z) = (cur - 5).clamp(0,127).toByte
								}
							}
//							TD.coveringGrid.modificationBlock( VoxelCoord(baseV.x + x,baseV.y + y,baseV.z + z) ) {
//								if ( TD.coveringGrid(baseV.x + x,baseV.y + y,baseV.z + z,5) != 0 ) {
//									TD.coveringGrid(baseV.x + x,baseV.y + y,baseV.z + z,5) = 0.toByte
//								}
//							}
						}
					}
				}
			}
	
			val looped = iter.advance()
//			if ( looped ) { println("loop completed") }
			if ( looped ) { countdown = 0 }
		}
	}
	def updateHeatSource ( heatSource : GameEntity ) {
		val HD = heatSource.aux[HeatSourceData]
		val position = heatSource match {
			case pe : TPhysicalEntity => pe.position.toVoxelCoord
			case _ => Noto.warn("non physical heat source..."); VoxelCoord.Sentinel
		}

		val TD = world.aux[TerrainData]
		val terrain = TD.materialGrid

		val heap = new FibonacciHeap[ThermalNode]
		heap.enqueue( ThermalNode(position.x,position.y,position.z,0.0f) )
		val closed = new LongOpenHashSet()

		val heatStrength = HD.heatStrength
		val falloff = 2

		val range = HD.heatStrength / falloff + 4
		val buckets = Array.fill(range) { new ArrayBuffer[ThermalNode] }
		
		val heatGrid = world.aux[ThermodynamicData].heatGridsBySource(heatSource).grid

		while ( heap.nonEmpty && heap.peek.g * falloff < HD.heatStrength ) {
			val head = heap.dequeue()
			val hash = VoxelCoord.hashL(head.x,head.y,head.z)

			if ( ! closed.contains(hash) ) {
				closed.add(hash)
				buckets( head.g.toInt / falloff ).append( head )

				for ( q <- 0 until expandedCardinals.length optimized ) {
					val ax = head.x + expandedCardinals(q).x
					val ay = head.y + expandedCardinals(q).y
					val az = head.z + expandedCardinals(q).z

					val aHash = VoxelCoord.hashL(ax,ay,az)
					if ( ! closed.contains(aHash) ) {
						val matByte = terrain(ax,ay,az)
						val insulation = if ( matByte == 0 ) { 1 } else { TD.materialMapping(matByte).insulation + 1 }
						val ag = head.g + insulation * expandedCardinalLength(q) 

						heap.enqueue(ThermalNode(ax,ay,az,ag))
					}
				}
			}
		}
		
		val ratios = buckets.map( _ => 1.0f )

		var limit = 1000000
		for ( i <- 0 until buckets.size optimized ) {
			for ( n <- buckets(i) ) {
				val v = clamp((heatStrength - n.g * falloff) * ratios(i),0.0f,limit).toShort
				limit = math.min(limit,v)
				heatGrid(n.x,n.y,n.z) = v
			}
		}
	}

	def queryResultAdded(t: GameEntity): Unit = {
		val ThermData = world.aux[ThermodynamicData]
		val channel = ThermodynamicData.ThermalChannel(new GenericTaleaGrid[Short,GenericTalea[Short]](0,(v:VoxelCoord) => new GenericTalea[Short] (v,0 )),t)
		ThermData.heatGridsBySource(t) = channel
		ThermData.heatGrids.put(channel)
		
		updateHeatSource(t)
	}

	def queryResultRemoved(t: GameEntity): Unit = {
		
	}
}

object ThermodynamicsGameComponent {
	case class ThermalNode ( x : Int , y : Int , z : Int , g : Float ) extends THasSortKey {
		def sortKey: Float = g
	}
}
