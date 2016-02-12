package arx.tmp.game.logic.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/31/14
 * Time: 4:13 PM
 */

import scalaxy.loops._

class MergedInfiniteVoxelView[T] (views : Array[TInfiniteVoxelView[T]], nullValue : T) extends TInfiniteVoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = {
		for (i <- 0 until views.length optimized) {
			val tmp = views(i)(x,y,z)
			if (tmp != nullValue) {
				return tmp
			}
		}
		nullValue
	}
}

class DifferenceInfiniteVoxelView[T](baseView : TInfiniteVoxelView[T], differenceView : TInfiniteVoxelView[T], nullValue : T) extends TInfiniteVoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = {
		if (differenceView(x,y,z) != nullValue) {
			nullValue
		} else {
			baseView(x,y,z)
		}
	}
}