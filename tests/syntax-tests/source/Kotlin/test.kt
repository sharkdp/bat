import kotlin.math.*

data class Example(
    val name: String,
    val numbers: List<Int?>
)

fun interface JokeInterface {
    fun isFunny(): Boolean
}

abstract class AbstractJoke : JokeInterface {
    override fun isFunny() = false
    abstract fun content(): String
}

class Joke : AbstractJoke() {
    override fun isFunny(): Boolean {
        return true
    }
    override fun content(): String = "content of joke here, haha"
}

class DelegatedJoke(val joke: Joke) : JokeInterface by joke {
    val number: Long = 123L

    companion object {
        const val someConstant = "some constant text"
    }
}

object SomeSingleton

sealed class Shape {
    abstract fun area(): Double
}

data class Square(val sideLength: Double) : Shape() {
    override fun area(): Double = sideLength.pow(2)
}

object Point : Shape() {
    override fun area() = .0
}

class Circle(val radius: Double) : Shape() {
    override fun area(): Double {
        return PI * radius * radius
    }
}

fun String.extensionMethod() = "test"

fun main() {
    val name = """
       multiline
       string
       
       some numbers: 123123 42
    """.trimIndent()
    val example = Example(name = name, numbers = listOf(512, 42, null, -1))

    example.numbers
        .filterNotNull()
        .forEach { println(it) }

    setOf(Joke(), DelegatedJoke(Joke()).joke)
        .filter(JokeInterface::isFunny)
        .map(AbstractJoke::content)
        .forEachIndexed { index: Int, joke ->
            println("I heard a funny joke(#${index + 1}): $joke")
        }

    listOf(Square(12.3), Point, Circle(5.2))
        .associateWith(Shape::area)
        .toList()
        .sortedBy { it.second }
        .forEach {
            println("${it.first}: ${it.second}")
        }

    println("some string".extensionMethod())

    require(SomeSingleton::class.simpleName == "SomeSingletonName") { "something does not seem right..." }
}
