package arx.tmp.game.logic.window

import arx.core.vec.Vec2i
import arx.tmp.game.logic.datastructures.CellGrid2D
import arx.tmp.game.logic.datastructures.GridCell2D
import arx.tmp.game.logic.datastructures.TGridCell2D
import arx.tmp.game.logic.datastructures.TTaleaGridWindow
import arx.tmp.game.logic.traits.Cell3D


/**
 *
 */

trait TValueView2D[T] { def apply ( x : Int, y : Int ) : T }
trait TValueStore2D[T] { def update ( x : Int, y : Int, z : T ) }

class GridCellView[@specialized(Int,Short,Byte) T](cell : TGridCell2D[T],grid : CellGrid2D[T]) extends TValueView2D[T] with TValueStore2D[T] {
	var offsetX = 0
	var offsetY = 0
	val scale = grid.scale
	def apply ( x : Int , y : Int ) : T = cell((x >> scale) + offsetX,(y >> scale) + offsetY)
	def update ( x : Int , y : Int , b : T ){ cell((x >> scale) + offsetX,(y >> scale) + offsetY) = b }
	def alignTo ( threecell : Cell3D ) {
		offsetX = (threecell.position.x >> scale) - cell.x
		offsetY = (threecell.position.y >> scale) - cell.y
	}
}

class CellGridWindow[T](center : Vec2i , grid : CellGrid2D[T]) extends TValueView2D[T] with TValueStore2D[T] {
	var cellMatrix = Array.ofDim[TGridCell2D[T]](11)
	var cells = Array.ofDim[TGridCell2D[T]](9)
	var offsetX: Int = 0
	var offsetY: Int = 0

	def alignTo ( threecell : Cell3D ) {
		offsetX = threecell.position.x - center.x
		offsetY = threecell.position.y - center.y
	}
	def alignTo ( window : TTaleaGridWindow[_,_] ) {
		offsetX = window.center.x - center.x
		offsetY = window.center.y - center.y
	}

	def rawCell( x : Int ,y : Int  ) : TGridCell2D[T] = {
		val rx = (x >> GridCell2D.dimensionPo2)
		val ry = (y >> GridCell2D.dimensionPo2)

		cellMatrix( cellHash(rx,ry) )
	}

	def apply ( x : Int ,y : Int ) : T = {
		val rx = ((x+offsetX))
		val ry = ((y+offsetY))
		val sx = (rx + GridCell2D.dimension) & GridCell2D.dimensionm1
		val sy = (ry + GridCell2D.dimension) & GridCell2D.dimensionm1
		rawCell(rx,ry)(sx,sy)
	}
	
	def update ( x : Int ,y : Int , b : T ) {
		val rx = ((x+offsetX))
		val ry = ((y+offsetY))
		val sx = (rx + GridCell2D.dimension) & GridCell2D.dimensionm1
		val sy = (ry + GridCell2D.dimension) & GridCell2D.dimensionm1
		rawCell(rx,ry)(sx,sy) = b
	}

	def cellHash (x: Int,y: Int) : Int = { ((x+1) << 2) + (y+1) }
	def initCells () {
		var i = 0
		var x = -1;while ( x <= 1 ) {
			var y = -1;while ( y <= 1 ) {
				val cell = grid.getCell( center.x + x * GridCell2D.dimension , center.y + y * GridCell2D.dimension )
				cellMatrix( cellHash(x,y) ) = cell
				cells(i) = cell
				i += 1
			y += 1}
		x += 1}
	}

	initCells()
}

object Window {
	def apply[T] ( center : Vec2i , grid : CellGrid2D[T] ) : CellGridWindow[T] = {
		new CellGridWindow[T]((center >> GridCell2D.dimensionPo2) << GridCell2D.dimensionPo2,grid)
	}
}