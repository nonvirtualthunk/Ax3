package arx.eldr.game.logic.light

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/7/12
 * Time: 12:07 PM
 * Created by nonvirtualthunk
 */


class FastLightNodeQueue {
	val power = 17
	var capAND = (1 << power) - 1
	var capacity = (1 << power)
	var front = 0
	var lastFront = 0
	var back = 0
	var backingArray = Array.ofDim[Int](capacity)

	def nonEmpty = front != back
	def enqueue( x : Int , y : Int , z : Int , cameFrom : Int , lightValue : Int) {
		backingArray(back+0) = x
		backingArray(back+1) = y
		backingArray(back+2) = z
		backingArray(back+3) = cameFrom | (lightValue << 10)
		back = (back + 4) & capAND
	}
	def dequeue() { lastFront = front; front = (front + 4) & capAND }

	def x = backingArray(lastFront)
	def y = backingArray(lastFront+1)
	def z = backingArray(lastFront+2)
	def cameFrom = (backingArray(lastFront+3) & 0x000000ff)
	def lightValue = (backingArray(lastFront+3) >> 10)

	def clear () { front = back }
}