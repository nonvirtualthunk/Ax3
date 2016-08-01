package arx.axistential.game.logic.ai;

import arx.core.THasSortKey;
import arx.core.vec.coordinates.MutableVoxelCoord;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/13
 * Time: 5:33 PM
 * To change this template use File | Settings | File Templates.
 */
final public class  RawSearchNode implements THasSortKey {
    public RawSearchNode(int _index) {
        this._index = _index;
    }

    public float sortKey() {
        return f();
    }

    public MutableVoxelCoord v = new MutableVoxelCoord();

    public int c;
    public int j;

    public float g;
    public float h;
    public float f () { return g + h; }
    public RawSearchNode parent;

    public int _index = 0;

    private int cachedHash;

    public RawSearchNode init ( int x_, int y_, int z_, int c_, int j_, float g_, float h_, RawSearchNode parent_) {
        v.x_$eq( x_ );
        v.y_$eq(y_);
        v.z_$eq(z_);
        c = c_;
        j = j_;
        g = g_;
        h = h_;
        parent = parent_;
        // precompute hash
        cachedHash = v.hashCode();
        cachedHash = 31 * cachedHash + c;
        cachedHash = 31 * cachedHash + j;
        return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RawSearchNode that = (RawSearchNode) o;

        if ( ! v.fastEquals(that.v) ) return false;
        if (c != that.c) return false;
        if (j != that.j) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return cachedHash;
    }
}
