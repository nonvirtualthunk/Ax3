package arx.tmp.game.logic.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/21/13
 * Time: 8:33 AM
 * Created by nonvirtualthunk
 */

trait TShadowGrid {
	def apply ( x : Int, y : Int, z : Int ) : Float
	def update ( x : Int, y : Int, z : Int , f : Float )
}

trait TInfiniteShadowGrid {
	def apply ( x : Int, y : Int, z : Int ) : Float
}