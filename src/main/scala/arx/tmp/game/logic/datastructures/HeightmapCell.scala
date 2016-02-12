package arx.tmp.game.logic.datastructures

import java.io._

import arx.core.vec.Vec2i
import arx.tmp.game.logic.traits.WorldCell

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/11
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */

object HeightmapCell{
	val DimensionPo2 = 7
	val Dimension = 1 << DimensionPo2
	def hashPreShifted(x: Int,y: Int): Int = { x + (y << 15) }
}

class HeightmapCell(val position: Vec2i) extends WorldCell with java.io.Externalizable {
	def this () {
		this(Vec2i(-1,-1))
	}
	var data = new Array[Int](HeightmapCell.Dimension * HeightmapCell.Dimension)
	var shiftedPosition: Vec2i = (position >> HeightmapCell.DimensionPo2)
	var hash = HeightmapCell.hashPreShifted(shiftedPosition.x,shiftedPosition.y)

	def apply(x: Int,y: Int): Int = { data(x + (y << HeightmapCell.DimensionPo2)) }
	def update(x: Int,y: Int,v: Int){ data(x + (y << HeightmapCell.DimensionPo2)) = v }

	def x = position.x
	def y = position.y


	@throws( classOf[IOException] )
	def writeExternal ( bout : ObjectOutput ) {
		bout.writeObject(data)
		bout.writeObject(shiftedPosition)
		bout.writeInt(hash)
	}

	@throws( classOf[IOException] )
	def readExternal ( bin : ObjectInput ) {
		data = bin.readAs[Array[Int]]
		shiftedPosition = bin.readAs[Vec2i]
		hash = bin.readInt
	}
}
object SentinelHeightmapCell extends HeightmapCell(Vec2i(-1,-1)){
	override def apply (x: Int,y: Int): Int = { -1 }
	def apply (): Byte = { -1 }
	override def update (x: Int,y: Int,b: Int){ }
}