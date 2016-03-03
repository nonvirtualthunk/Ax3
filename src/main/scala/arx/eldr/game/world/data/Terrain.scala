package arx.eldr.game.world.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 3/3/16
  * Time: 8:07 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.VoxelGrid
import scalaxy.loops._
import arx.core.vec._

class Terrain {
	val material = new VoxelGrid[Byte](0.toByte)
	// so we do have the question here, how do we represent both actual materials, and the
	// pseudo-materials that are occupation of a voxel by an object or something. The old
	// approach was to use [1,127] to represent materials, [-1,-127] to represent psuedos
	// by checking the negative bit, then using each other bit to represent a single
	// characteristic (light passable, physically passable, etc). That gives us seven bits
	// of information we can use. We currently have six bits that we use in anth,
	// (Passable,ConditionallyPassable,FluidPassable,Climbable,Transparent,PartialTransparent)
	// My concern there is that we might well run out of bits, we only have one more in
	// terms of headroom. The other alternative would be to use pseudo-materials the same
	// way we do regular materials, by the ByteMapping, they would just have a special
	// flag that indicates that they are pseudos, then we're limited by the unique combinations
	// of flags, rather than the bits themselves, and given that some are mutually exclusive,
	// like Transparent and PartiallyTransparent, that could be a boon, we also do have
	// plans for extending the range beyond 255 slots by having an open ended second grid,
	// though that would be somewhat more complicated, and we should probably plan ahead of
	// time. Alternately, a talea-level byte mapping would solve our problems pretty well.

	// The other possibility would be to have two separate grids, a material grid to hold
	// just the actual material occupied blocks, and a separate grid that represents purely
	// the characteristics of each voxel (including those occupied by a material), that
	// grid would then be the one accessed by pretty much everything other than drawing.
	// Downside there, two writes every time, and it only buys us an extra bit of
	// characteristics, almost certainly not worth it.

	// So we should, I think, have a look at accounting for the possibility of extending
	// beyond 255 options. The simplest solution of course would be to use shorts instead
	// of bytes, that would increase our material selection to the point of never running
	// out, ever. It would, however, double our space usage for materials and probably
	// lower our performance (just due to the fact that it is reading more memory, and
	// that is our primary limitation)
}