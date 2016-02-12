package arx.tmp.game.logic.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/1/14
 * Time: 4:02 PM
 */

class FunctionVoxelView[T](func : (Int,Int,Int)=>T) extends TInfiniteVoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = func(x,y,z)
}
