package arx.tmp.game.logic.datastructures

import java.io._

import arx.Prelude
import arx.core.vec.Vec2i._
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.traits.Cell2D
import arx.tmp.game.logic.window.CellGridWindow
import arx.tmp.game.logic.window.GridCellView
import arx.tmp.game.logic.window.TValueStore2D
import arx.tmp.game.logic.window.TValueView2D
import arx.tmp.game.logic.world.SpatialRegion

//Given a 2048 x 2048 world of bytes, base memory is 4 megabytes
//at resolution 2 - 1 megabyte
//at resolution 4 - 0.25 megabyte
//for integers, x4
@SerialVersionUID(1L)
class CellGrid2D[T](var scaleDenominator : Int = 0)(implicit var providedManifest : Manifest[T]) extends Externalizable {
	def this () { this(0)(null) }
	type CellType = TGridCell2D[T]
	var cells = overlock.atomicmap.AtomicMap.atomicNBHM[Int,CellType]
	var activeCell = if ( providedManifest != null ) { new GridCell2D[T](Vec2i(-1,-1))(providedManifest) } else { null }
	var scale = if ( scaleDenominator == 0 ) { 0 } else { (math.log(scaleDenominator) / math.log(2)).toInt }

	protected def cellFor ( x : Int ,y : Int ) : CellType = {
		val c = activeCell
		val sx = x >> GridCell2D.dimensionPo2
		val sy = y >> GridCell2D.dimensionPo2
		if ( c.shiftedPosition.x == sx && c.shiftedPosition.y == sy ) { c }
		else { cells.getOrElseUpdate( GridCell2D.hashPreShifted(sx,sy) , GridCell2D( Vec2i(sx << GridCell2D.dimensionPo2,sy << GridCell2D.dimensionPo2) )(providedManifest) ) }
	}
	def apply(v: ReadVec2i): T = apply(v.x,v.y)
	def update(v: ReadVec2i,b: T){ update(v.x,v.y,b) }
	def apply(bx: Int,by: Int): T = {
		val x = bx >> scale
		val y = by >> scale
		val c = cellFor(x,y)
		c(x - c.x,y - c.y)
	}
	def update(bx: Int,by: Int,b: T){
		val x = bx >> scale
		val y = by >> scale
		val c = cellFor(x,y)
		c(x - c.x,y - c.y) = b
	}

	def getCell (bx: Int,by: Int): CellType = {
		Prelude.posit(scale == 0,"using rawGetCell when scale is non-zero, individual cells are not adjusted by scale")
		val x = bx >> scale
		val y = by >> scale
		cellFor(x,y)
	}

	def getCell_internal (bx: Int,by: Int): CellType = {
		val x = bx >> scale
		val y = by >> scale
		cellFor(x,y)
	}

	def cellView ( v : ReadVec2i ) : GridCellView[T] = { cellView(v.x,v.y) }
	def cellView ( bx : Int , by : Int ) : GridCellView[T] = {
		new GridCellView(getCell_internal(bx,by),this)
	}

	/**
	 * Returns all taleae that have at least one voxel in the specified region
	 */
	def allInRegionInclusive ( region : SpatialRegion ): Set[CellType] = {
		var ret = Set[CellType]()
		val start = region.lowerCorner >> scale
		val end = region.higherCorner >> scale

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ){
			val location = Vec2i(x << Talea.dimensionPo2,y << Talea.dimensionPo2)
			ret = ret + cellFor(location.x,location.y)
		}
		ret
	}

	def allInRegionInclusiveOrdered ( region : SpatialRegion ): List[CellType] = {
		var ret = List[CellType]()
		val start = region.lowerCorner >> scale
		val end = region.higherCorner >> scale

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ){
			val location = Vec2i(x << Talea.dimensionPo2,y << Talea.dimensionPo2)
			ret = cellFor(location.x,location.y) :: ret
		}
		ret
	}

	def allInRegionInclusive ( regions : Seq[SpatialRegion] ): Set[CellType] = {
		regions.foldRight(Set[CellType]())( allInRegionInclusive(_) ++ _ )
	}

	def foreachPar ( func : (CellType) => Unit ) {
		cells.values.par.foreach( func )
	}
	def par = cells.values.par

	def windowCenteredAt ( loc : ReadVec2i ) = {
		new CellGridWindow[T](loc,this)
	}

	@throws( classOf[IOException] )
	def writeExternal ( bout : ObjectOutput ) {
		bout.writeNBHM(cells)
		bout.writeObject(providedManifest)

	}

	@throws( classOf[IOException] )
	def readExternal ( bin : ObjectInput ) {
		cells = bin.readNBHM[Int,CellType]
		providedManifest = bin.readAs[Manifest[T]]
		activeCell = new GridCell2D[T](Vec2i(-1,-1))(providedManifest)
	}
}

trait TGridCell2D[T] extends Cell2D with TValueView2D[T] with TValueStore2D[T] with Serializable {

}

@SerialVersionUID(1L)
class GridCell2D[T] (pos : ReadVec2i)(implicit providedManifest : Manifest[T]) extends TGridCell2D[T] {
//	def this () { this(Vec2i(-1,-1))(null) }
	var position = pos
	var shiftedPosition = position >> GridCell2D.dimensionPo2
	var _modifiedCount = 0
	var data = providedManifest.newArray( GridCell2D.dimension * GridCell2D.dimension )
	var hash = GridCell2D.hashPreShifted(shiftedPosition.x,shiftedPosition.y)
	private val Po2 = GridCell2D.dimensionPo2

	def dimensions = GridCell2D.dimension
	def dimension = GridCell2D.dimension

	
	def apply ( a : Int , b : Int ) : T = {
		Prelude.posit(a >= 0 && a < GridCell2D.dimension && b >= 0 && b < GridCell2D.dimension , "Invalid cell access : " + a + " , " + b);
		data( (b << Po2) + a ) }
	def update ( a : Int , b : Int , v : T ) { data( (b << GridCell2D.dimensionPo2) + a ) = v }

	def x = position.x
	def y = position.y

	def modifiedCount = _modifiedCount
	def modifiedCount_=(i: Int) { _modifiedCount = i }
}

@SerialVersionUID(1L)
class GridCell2Di (pos : ReadVec2i) extends TGridCell2D[Int] {
	val position = pos
	val shiftedPosition = position >> GridCell2D.dimensionPo2
	private[this] val data = Array.ofDim[Int]( GridCell2D.dimension * GridCell2D.dimension )
	val hash = GridCell2D.hashPreShifted(shiftedPosition.x,shiftedPosition.y)
	var _modifiedCount = 0
	private[this] val Po2 = GridCell2D.dimensionPo2

	def dimensions = GridCell2D.dimension
	def dimension = GridCell2D.dimension


	def apply ( a : Int , b : Int ) : Int = {
		Prelude.posit(a >= 0 && a < GridCell2D.dimension && b >= 0 && b < GridCell2D.dimension , "Invalid cell access : " + a + " , " + b);
		data( (b << Po2) + a ) }
	def update ( a : Int , b : Int , v : Int ) { data( (b << GridCell2D.dimensionPo2) + a ) = v }


	def getShiftedPosition = shiftedPosition
	def x = position.x
	def y = position.y

	def modifiedCount = _modifiedCount
	def modifiedCount_=(i: Int) { _modifiedCount = i }
}


object GridCell2D {
	def apply[T] ( pos : ReadVec2i )(man : Manifest[T]) : TGridCell2D[T] = {
		if ( man.runtimeClass == classOf[Int] ) {
			new GridCell2Di(pos).asInstanceOf[TGridCell2D[T]]
		} else {
			new GridCell2D[T](pos)(man)
		}
	}

	val dimensionPo2 = 7
	val dimension = 1 << dimensionPo2
	val dimensionm1 = dimension - 1
	def hash ( a : Int , b : Int ) : Int = { ((a >> GridCell2D.dimensionPo2) << 15) + (b >> GridCell2D.dimensionPo2) }
	def hashPreShifted ( a : Int , b : Int ) : Int = { ((a) << 15) + (b) }
}

object CellGrid2D{
	def apply[T : Manifest] () = new CellGrid2D[T](0)(manifest[T])
	def apply[T : Manifest] ( denom : Int ) = new CellGrid2D[T](denom)(manifest[T])
	def allTaleaeRegionsInRegion (region: SpatialRegion): List[SpatialRegion] = {
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

class BoundedGridCell2D[@specialized(Int,Short,Byte) T](baseCell : GridCell2D[T], grid : CellGrid2D[T] ) {
	def apply ( x_b : Int , y_b : Int ) : T = {
		val x = x_b >> grid.scale
		val y = y_b >> grid.scale
		if ( x >= 0 && y >= 0 && x < GridCell2D.dimension && y < GridCell2D.dimension ) {
			baseCell(x,y)
		} else {
			val nc = grid.getCell( baseCell.x + x , baseCell.y + y )
			nc( (baseCell.x + x) - nc.x , (baseCell.y + y) - nc.y )
		}
	}
	def update ( x_b : Int , y_b : Int , b : T ) {
		val x = x_b >> grid.scale
		val y = y_b >> grid.scale
		if ( x >= 0 && y >= 0 && x < GridCell2D.dimension && y < GridCell2D.dimension ) {
			baseCell(x,y) = b
		} else {
			val nc = grid.getCell( baseCell.x + x , baseCell.y + y )
			nc( (baseCell.x + x) - nc.x , (baseCell.y + y) - nc.y ) = b
		}
	}
}