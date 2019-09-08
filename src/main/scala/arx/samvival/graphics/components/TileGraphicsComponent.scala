package arx.samvival.graphics.components

import arx.core.vec.coordinates.{AxialVec, CubeVec}
import arx.core.vec.{Vec2f, Vec3f}
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.graphics.helpers.HSBA
import arx.samvival.game.entities.{Terrain, Tile, Tiles}
import arx.samvival.graphics.data.CullingData

import scala.language.postfixOps

class TileGraphicsComponent(engine : LGraphicsEngine) extends SamvivalCanvasGraphicsComponent(engine) {
	drawOrder = Layers.Tile

	val cullingRevisionWatcher = engine.graphicsWorld[CullingData].createRevisionWatcher
	var hadOverlay = false

	override def needsUpdate: Boolean = {
		cullingRevisionWatcher.hasChanged || world.dataStore[Terrain].hasOverlay || hadOverlay
	}

	override def draw(canvas: SVCanvas): Unit = {
		hadOverlay = world.dataStore[Terrain].hasOverlay
		val cullData = engine.graphicsWorld[CullingData]

		for (hex <- cullData.hexesByCartesianCoord) {
			val tile = Tiles.tileAt(hex)
			if (world.hasData[Tile](tile) && world.hasData[Terrain](tile)) {
				val terrain = world.data[Terrain](tile)

				canvas.quad(hex, Layers.Tile)
					.withColor(HSBA(0.0f, 0.0f, 1.0f, 1.0f))
					.withTexture(s"samvival/terrain/${terrain.kind.name}.png")
					.withDimensions(Vec2f(142.0f, 124.0f))
					.withLightColor(Vec3f.One)
					.draw()
			}
		}
	}
}

case class HexRingIterator(axialCenter : AxialVec, radius : Int) extends Iterator[AxialVec] {
	val center = axialCenter.asCubeVec + CubeVec.CubeDelta(4) * radius
	var i = 0
	var j = 0
	var cur = center

	override def hasNext: Boolean = {
		(radius == 0 && i == 0 && j == 0) || (i< 6 && j < radius)
	}

	override def next(): AxialVec = {
	 	if (radius == 0) {
			if (i == 0 && j == 0) {
				j = 1
				return cur.asAxialVec
			}
		} else {
			if (i < 6) {
				if (j < radius) {
					val ret = cur
					cur = cur + CubeVec.CubeDelta(i)
					j += 1
					if (j >= radius) {
						j = 0
						i += 1
					}
					return ret.asAxialVec
				}
			}
		}
		throw new IllegalStateException("Empty ring iterator")
	}
}

/*
struct RingIterator {
    radius : u32,
    i : usize,
    j : u32,
    cur : CubeCoord
}
impl Iterator for RingIterator {
    type Item = CubeCoord;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut ret = None;
        if self.radius == 0 {
            if self.i == 0 && self.j == 0 {
                self.i += 1;
                ret = Some(self.cur)
            }
        } else {
            if self.i < 6 {
                if self.j < self.radius {
                    ret = Some(self.cur);
                    self.cur = self.cur + CUBE_DELTAS[self.i];
                    self.j = self.j + 1;
                    if self.j >= self.radius {
                        self.j = 0;
                        self.i += 1;
                    }
                }
            }
        }
        ret
    }
}
 */