package zone.frog.advent.twentytwo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import zone.frog.advent.twentytwo.Day25BruteForce.decimalToSnafu
import zone.frog.advent.twentytwo.Day25BruteForce.scenarioOne
import zone.frog.advent.twentytwo.Day25BruteForce.snafuToDecimal

class Day25BruteForceTest {
    @Test
    fun testDecimal() {
        val mapping = mapOf(
            1L to "1",
            2L to "2",
            3L to "1=",
            4L to "1-",
            5L to "10",
            6L to "11",
            7L to "12",
            8L to "2=",
            9L to "2-",
            10L to "20",
            15L to "1=0",
            20L to "1-0",
            2022L to "1=11-2",
            12345L to "1-0---0",
            314159265L to "1121-1110-1=0"
        )

        mapping.forEach {
            assertEquals(it.value, decimalToSnafu(it.key), "${it.key} -> ${it.value}")
            assertEquals(it.key, snafuToDecimal(it.value), "${it.value} -> ${it.key}")
        }
    }

    @Test
    fun testSnafu() {
        val mapping = mapOf(
            "1=-0-2" to 1747L,
            "12111" to 906L,
            "2=0=" to 198L,
            "21" to 11L,
            "2=01" to 201L,
            "111" to 31L,
            "20012" to 1257L,
            "112" to 32L,
            "1=-1=" to 353L,
            "1-12" to 107L,
            "12" to 7L,
            "1=" to 3L,
            "122" to 37L,
        )
        mapping.forEach {
            assertEquals(it.value, snafuToDecimal(it.key), "${it.key} -> ${it.value}")
            assertEquals(it.key, decimalToSnafu(it.value), "${it.value} -> ${it.key}")
        }
        assertEquals(4890, mapping.keys.sumOf { snafuToDecimal(it) })
    }

    @Test
    internal fun relevantTest() {
        assertEquals("2=-1=0", decimalToSnafu(4890))
    }

    @Test
    internal fun testScenarioOne() {
        assertEquals("2-02===-21---2002==0", scenarioOne("../input/day25.txt"))
    }
}