package arx.tmp.game.logic.datastructures

import java.io._

import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.world.SpatialRegion

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/11
 * Time: 2:53 PM
 * To change this template use File | Settings | File Templates.
 */

@SerialVersionUID(1L)
class Heightmap extends Externalizable {
	var m_taleae = overlock.atomicmap.AtomicMap.atomicNBHM[Int,HeightmapCell]
	var m_activeHeightmapCell = new ThreadLocal[HeightmapCell] {
		override def initialValue:HeightmapCell = {
			SentinelHeightmapCell
		}
	}

	var defaultValue = 0


	def apply(v: Vec2i): Int = apply(v.x,v.y)
	def update(v: Vec2i,b: Int){ update(v.x,v.y,b) }
	def apply(x: Int,y: Int): Int = {
		m_activeHeightmapCell.set(fetchActiveHeightmapCell(x,y))
		m_activeHeightmapCell.get()(x - m_activeHeightmapCell.get.x,y - m_activeHeightmapCell.get.y)
	}
	def update(x: Int,y: Int,b: Int){
		m_activeHeightmapCell.set(fetchActiveHeightmapCell(x,y))
		m_activeHeightmapCell.get()(x - m_activeHeightmapCell.get.x,y - m_activeHeightmapCell.get.y) = b
	}

	def fetchActiveHeightmapCell (x: Int,y: Int): HeightmapCell = {
		val nx = x >> HeightmapCell.DimensionPo2
		val ny = y >> HeightmapCell.DimensionPo2
		if ( ((nx) == (m_activeHeightmapCell.get.shiftedPosition.x)) &&
			 ((ny) == (m_activeHeightmapCell.get.shiftedPosition.y)) )
		{
			m_activeHeightmapCell.get
		}
		else{
			m_taleae.getOrElseUpdate( 	HeightmapCell.hashPreShifted(nx,ny) ,
										new HeightmapCell(Vec2i(nx,ny) << HeightmapCell.DimensionPo2) )
		}
	}

	def setTalea (cell : HeightmapCell){
		m_taleae.put(cell.hash,cell)
	}

	def rawGetHeightmapCell (x: Int,y: Int): HeightmapCell = {
		val nx = x >> HeightmapCell.DimensionPo2
		val ny = y >> HeightmapCell.DimensionPo2
		if ( ((nx) == (m_activeHeightmapCell.get.shiftedPosition.x)) &&
			 ((ny) == (m_activeHeightmapCell.get.shiftedPosition.y)) )
		{
			m_activeHeightmapCell.get
		}
		else{
			m_taleae.getOrElseUpdate( 	HeightmapCell.hashPreShifted(nx,ny) ,
										new HeightmapCell(Vec2i(nx,ny) << HeightmapCell.DimensionPo2) )
		}
	}

	/**
	 * Returns all taleae that have at least one voxel in the specified region
	 */
	def allInRegionInclusive ( region : SpatialRegion ): Set[HeightmapCell] = {
		var ret = Set[HeightmapCell]()
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x until shiftedEnd.x; y <- shiftedStart.y until shiftedEnd.y ){
			val location = Vec2i(x << Talea.dimensionPo2,y << Talea.dimensionPo2)
			ret = ret + rawGetHeightmapCell(location.x,location.y)
		}
		ret
	}

	def allInRegionInclusive ( regions : Seq[SpatialRegion] ): Set[HeightmapCell] = {
		regions.foldRight(Set[HeightmapCell]())( allInRegionInclusive(_) ++ _ )
	}


	@throws( classOf[IOException] )
	def writeExternal ( bout : ObjectOutput ) {
		bout.writeNBHM(m_taleae)
		bout.writeInt(defaultValue)
	}

	@throws( classOf[IOException] )
	def readExternal ( bin : ObjectInput ) {
		m_taleae = bin.readNBHM[Int,HeightmapCell]
		defaultValue = bin.readInt
	}
}

object Heightmap{
	def allHeightmapRegionsInRegion (region: SpatialRegion): List[SpatialRegion] = {
		var ret = List[SpatialRegion]()
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> HeightmapCell.DimensionPo2
		val shiftedEnd = end >> HeightmapCell.DimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ; z <- shiftedStart.z to shiftedEnd.z ){
			val location = Vec3i(x << HeightmapCell.DimensionPo2,y << HeightmapCell.DimensionPo2,z << HeightmapCell.DimensionPo2)
			val taleaRegion = SpatialRegion.fromCorners(location,location + HeightmapCell.Dimension)
			ret ::= region.intersection(taleaRegion)
		}
		ret
	}
}