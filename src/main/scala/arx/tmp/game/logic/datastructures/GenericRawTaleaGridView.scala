package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/13/12
 * Time: 1:05 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec3i

class GenericRawTaleaGridView[@specialized(Byte,Short,Int) T] ( grid : TTaleaGrid[T,_] , offset : ReadVec3i ) {
	def apply ( x : Int ,y : Int , z : Int ) : T = {
		grid(x + offset.x,y + offset.y,z + offset.z)
	}
}