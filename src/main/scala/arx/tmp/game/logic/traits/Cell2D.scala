package arx.tmp.game.logic.traits

import arx.core.vec.ReadVec2i

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/11
 * Time: 4:01 PM
 * To change this template use File | Settings | File Templates.
 */

trait Cell2D extends Cell {
	def position : ReadVec2i
	def shiftedPosition : ReadVec2i

	def x: Int
	def y: Int
}