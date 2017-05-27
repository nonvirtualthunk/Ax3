package arx.terragen.terrain.data

import java.util.concurrent.ThreadLocalRandom

import arx.Prelude
import arx.Prelude._
import arx.core.datastructures.FiniteGrid2D
import arx.core.datastructures.Voronoi
import arx.core.math.Rectf
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2i
import arx.core.vec.VecFunc

/**
  * TODO: Add javadoc
  */

class HeightData {
	val heightmap = FiniteGrid2D[Short](Vec2i(100,100), 0.toShort)
}



trait GenerationProcessor[In,Out] {
	def transform(in : List[In]) : List[Out]
}

trait GenerationSource[T] {
	private val _rand = ThreadLocalRandom.current()
	def random = _rand

	def generate(n : Int) : Traversable[T]
}

class RandomPointSource2D(bounds : Rectf) extends GenerationSource[ReadVec2f] {
	override def generate(n : Int): List[ReadVec2f] = {
		fillList(n)(_ => ReadVec2f(bounds.x + random.nextFloat() * bounds.width, bounds.y + random.nextFloat() * bounds.height))
	}
}


//
//class VoronoiProcessor extends GenerationProcessor[ReadVec2f, ReadVec2f] {
//
//	override def transform(in: List[ReadVec2f]): List[ReadVec2f] = {
//		val (min,max) = VecFunc.minmax2f(in)
//		val voronoi = Voronoi.fromPoints(in, Rectf.fromMinAndMax(min,max))
//		val res = voronoi.regions
//		res.foreach(r => r.edges)
//	}
//}