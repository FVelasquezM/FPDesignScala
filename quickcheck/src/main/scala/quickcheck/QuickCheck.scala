package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for 
      v <- arbitrary[A]
      heap <- oneOf(const(empty), genHeap)
    yield
      insert(v, heap)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min") = forAll { (i: Int, j: Int) => 
    val h = insert(i, insert(j, empty))
    findMin(h) == (i min j)
  }
  
  property("empty") = forAll { (i: Int) =>  
    isEmpty(deleteMin(insert(i, empty)))
  }

  property("sorting") = forAll {(h: H) => 
    def iter(h: H): Boolean = 
      if(isEmpty(h)) true
      else
        val next = deleteMin(h)
        isEmpty(next) || findMin(h) < findMin(next) && iter(next) 
    
    iter(h)
  }

  property("melding") = forAll{ (h1: H, h2: H) => 
    val min1 = if(isEmpty(h1)) 0 else findMin(h1)
    val min2 = if(isEmpty(h2)) 0 else findMin(h2)

    val min = findMin(meld(h1, h2))
    min == min1 || min == min2
  }


