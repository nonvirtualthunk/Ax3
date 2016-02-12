package arx.tmp.game.logic.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 12:39 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.THasSortKey
import arx.core.vec.coordinates.VoxelCoord

case class AStarSearchNode ( v : VoxelCoord ,var g : Float , h : Float, var parent : AStarSearchNode ) extends THasSortKey {
	def f = g + h
	def sortKey: Float = f
	def path : List[VoxelCoord] = parent match {
		case null => List(v)
		case p => p.path :+ v
	}
}