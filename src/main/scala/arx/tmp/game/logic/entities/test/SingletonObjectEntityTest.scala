package arx.tmp.game.logic.entities.test

import arx.tmp.game.logic.entities.core.GameEntity


/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/30/11
 * Time: 8:08 PM
 * Created by nonvirtualthunk
 */


class SingletonObjectEntityTest {
	class A extends GameEntity{
		A.map = A.map + (this -> 3)
	}
	object A{
		var map = Map[A,Int]()
	}
	object SubA extends A {}
	object SubB extends A {}

	object Q { SubA;SubB }
}

trait TestTrait[T <: AnyRef]{
	var t: T = null.asInstanceOf[T];
}

class TestClass extends TestTrait[TestClass]{
	
}