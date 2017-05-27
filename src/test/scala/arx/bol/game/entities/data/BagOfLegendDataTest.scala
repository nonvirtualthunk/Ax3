package arx.bol.game.entities.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.geometry.Rectangle
import arx.core.math.Recti
import arx.core.vec.Vec2i
import org.scalatest.FlatSpec

import scalaxy.loops._

class BagOfLegendDataTest extends FlatSpec {


	"A rectangular shape" should "be bounded by itself and occupy all inside its bounds" in {
		val rect = Rectangle(3, 5)
		require(rect.bounds == Recti(0,0,3,5))
		for (x <- 0 until 3; y <- 0 until 5) {
			require(rect.isOccupied(x,y))
		}
	}

	"A union of two rectangular shapes" should "be bounded by the max enclosing, and occupy the union of their interior" in {
		val rectA = Rectangle(1,5)
		val rectB = Rectangle(3,1)
		val union = ShapeUnion(rectA, Vec2i(1,0), rectB, Vec2i(0,1))

		require(union.bounds == Recti(0,0,3,5))
		for (y <- 0 until 5) {
			require(union.isOccupied(1,y))
		}
		for (x <- 0 until 3) {
			require(union.isOccupied(x,1))
		}
		require(!union.isOccupied(0,0))
		require(!union.isOccupied(2,0))
		require(!union.isOccupied(0,3))
	}
}
