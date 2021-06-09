// DO NOT MODIFY THIS FILE
package repls

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultiSetTests extends TestsBase {
    test("MultiSets can represent the empty Set") {
        assertResult(Seq.empty) {
            MultiSet(Seq.empty).toSeq
        }
    }

    test("MultiSets should be generic") {
        assertResult(Seq('a')) {
            MultiSet(Seq('a')).toSeq
        }

        assertResult(Seq("element")) {
            MultiSet(Seq("element")).toSeq
        }

        assertResult(Seq(1)) {
            MultiSet(Seq(1)).toSeq
        }

        assertResult(Seq(1.0)) {
            MultiSet(Seq(1.0)).toSeq
        }
    }

    test("MultiSets have multiplicity") {
        assertResult(Seq('a','b','c')) {
            MultiSet(Seq('a','b','c')).toSeq.sorted
        }

        assertResult(Seq('a','a','a')) {
            MultiSet(Seq('a','a','a')).toSeq.sorted
        }

        assertResult(Seq(1,1,2,2,3)) {
            MultiSet(Seq(1,1,2,2,3)).toSeq.sorted
        }
    }

    test("MultiSets have the correct string representation") {
        assertResult("{}") {
            MultiSet(Seq.empty).toString
        }

        assertResult("{1,1,2}") {
            MultiSet(Seq(1,1,2)).toString
        }

        assertResult("{a,a,a,b,c}") {
            MultiSet(Seq('a','b','c','a','a')).toString
        }
    }

    test("MultiSets can be added") {
        assertResult(Seq('a')) {
            val singleElement = MultiSet(Seq('a'))
            val emptySet = MultiSet[Char](Seq.empty)

            (emptySet + singleElement).toSeq.sorted
        }

        assertResult(Seq(1,1,2,3)) {
            val emptySet = MultiSet[Int](Seq.empty)
            val longerMultiSet = MultiSet(Seq(1,1,2,3))

            (emptySet + longerMultiSet).toSeq.sorted
        }

        assertResult(Seq(1.0,1.0,1.2,1.2,2.0,2.2,4.0)) {
            val first = MultiSet(Seq(1.0,1.2,2.0))
            val second = MultiSet(Seq(1.0,1.2,2.2,4.0))

            (first + second).toSeq.sorted(Ordering.Double.TotalOrdering)
        }

        assertResult(Seq(1,2,2,3,4,5,5)) {
            val first = MultiSet(Seq(1,2))
            val second = MultiSet(Seq(2,4,5))
            val third = MultiSet(Seq(3,5))

            (first + second + third).toSeq.sorted
        }
    }

    test("MultiSets can be subtracted") {
        assertResult(Seq('a')) {
            val singleElement = MultiSet(Seq('a'))
            val emptySet = MultiSet[Char](Seq.empty)

            (singleElement - emptySet).toSeq.sorted
        }

        assertResult(Seq(1,1,2,3)) {
            val longerMultiSet = MultiSet(Seq(1,1,2,3))
            val emptySet = MultiSet[Int](Seq.empty)

            (longerMultiSet - emptySet).toSeq.sorted
        }

        assertResult(Seq.empty) {
            val emptySet = MultiSet[Int](Seq.empty)
            val singleElement = MultiSet(Seq(1))

            (emptySet - singleElement).toSeq.sorted
        }

        assertResult(Seq(2.0)) {
            val first = MultiSet(Seq(1.0,1.2,2.0))
            val second = MultiSet(Seq(1.0,1.2,2.2,4.0))

            (first - second).toSeq.sorted(Ordering.Double.TotalOrdering)
        }

        assertResult(Seq(1)) {
            val first = MultiSet(Seq(1,2,3))
            val second = MultiSet(Seq(2,4,5))
            val third = MultiSet(Seq(3,5))

            (first - second - third).toSeq.sorted
        }
    }

    test("MultiSets have intersections") {
        assertResult(Seq.empty) {
            val singleElement = MultiSet(Seq('a'))
            val emptySet = MultiSet[Char](Seq.empty)

            (singleElement * emptySet).toSeq.sorted
        }

        assertResult(Seq.empty) {
            val longerMultiSet = MultiSet(Seq(1,1,2,3))
            val emptySet = MultiSet[Int](Seq.empty)

            (longerMultiSet * emptySet).toSeq.sorted
        }

        assertResult(Seq.empty) {
            val emptySet = MultiSet[Int](Seq.empty)
            val singleElement = MultiSet(Seq(1))

            (emptySet * singleElement).toSeq.sorted
        }

        assertResult(Seq(1.0,1.2)) {
            val first = MultiSet(Seq(1.0,1.2,2.0))
            val second = MultiSet(Seq(1.0,1.2,2.2,4.0))

            (first * second).toSeq.sorted(Ordering.Double.TotalOrdering)
        }

        assertResult(Seq(2)) {
            val first = MultiSet(Seq(1,2,3))
            val second = MultiSet(Seq(2,4,5))
            val third = MultiSet(Seq(2,3,5))

            (first * second * third).toSeq.sorted
        }

        assertResult(Seq(2,3)) {
            val first = MultiSet(Seq(1,2,2,3,3,4,4,4))
            val second = MultiSet(Seq(2,3,5,5,5))

            (first * second).toSeq.sorted
        }
    }

    test("MultiSets can be used in expressions") {
        assertResult(Seq('a','a','d')) {
            val first = MultiSet(Seq('a','a','b','c','d','d','e'))
            val second = MultiSet(Seq('a','b','c','c','c','d','f','g','g'))
            val third = MultiSet(Seq('a','b','g','h','h'))
            val fourth = MultiSet(Seq('a','b','b','c','c','c','d','e','g'))

            (first + second * third - fourth).toSeq.sorted
        }

        assertResult(Seq(2,3)) {
            val first = MultiSet(Seq(1,2,3,4,4,4))
            val second = MultiSet(Seq(1,4,5,5,5))
            val third = MultiSet(Seq(1,1,1,2,3,3,4,5))
            val fourth = MultiSet(Seq(1,4,6,7,7,8))

            ((first - second) * (third - fourth)).toSeq.sorted
        }

        assertResult(Seq('d','d','i','o')) {
            val first = MultiSet(Seq('d','d','d','g','h','i','i','i','o'))
            val second = MultiSet(Seq('a','b','d','g','i','i'))
            val third = MultiSet(Seq('d','d','d','h','i','l','o','o'))
            val fourth = MultiSet(Seq('d','d','i','i','o','o','p','p'))

            ((first - second) * third * fourth).toSeq.sorted
        }
    }
}
