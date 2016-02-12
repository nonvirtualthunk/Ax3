package arx.tmp.game.logic.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 12:38 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.THasSortKey
import arx.core.vec.coordinates.VoxelCoord

case class SearchNode ( v : VoxelCoord , g : Float , parent : SearchNode ) extends THasSortKey {
	def sortKey: Float = g
	def path : List[VoxelCoord] = parent match {
		case null => List(v)
		case p => p.path :+ v
	}
}