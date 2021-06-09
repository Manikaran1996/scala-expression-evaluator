// DO NOT MODIFY THIS FILE
package repls

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultiSetReplTests extends TestsBase {
  test("The MultiSetRepl should echo resolved expressions") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{a}") {
      repl.readEval("{a}")
    }

    assertResult("{a,a}") {
      repl.readEval("{a,a}")
    }

    assertResult("{a,b,c}") {
      repl.readEval("{a,b,c}")
    }

    assertResult("{a,b}") {
      repl.readEval("( {a,b} )")
    }
  }

  test("The MultiSetRepl should evaluate summations") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{a,a,a,b}") {
      repl.readEval("{a,a} + {a,b}")
    }

    assertResult("{a,a,b,b,b,b,c,c}") {
      repl.readEval("{b,c} + {a,b,b} + {a,b,c}")
    }

    assertResult("{a,a,b,b,b,b,b,b,h,h,i,i,u,u,v,y,y,y,y}") {
      repl.readEval("{a,b,y,y} + {u,u,i} + {i,y,b,b,h} + {b,b,h,y} + {a,b,v}")
    }

    assertResult("{a,a,a,b,b,b,c,c,c,c}") {
      repl.readEval("{a,b,c} + ( {a,a} + {b,b,c,c,c} )")
    }

    assertResult("{a,a,b,b,b,c,f,f,g,g,h,i,r}") {
      repl.readEval("( {a,b} + {b,c,f} ) + ( {a,b} + {r,f,g} + {g,h,i} )")
    }
  }

  test("The MultiSetRepl should evaluate subtractions") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{}") {
      repl.readEval("{a} - {a}")
    }

    assertResult("{}") {
      repl.readEval("{a} - {a,b}")
    }

    assertResult("{c}") {
      repl.readEval("( ( {a,b,c} - {a} ) - {a,b} ) - {m,n}")
    }

    assertResult("{a,e}") {
      repl.readEval("{a,e,e,e,e,d} - ( {b,d,e,e,e} - {a,b,c} )")
    }

    assertResult("{a,t}") {
      repl.readEval("( {a,a,a,a,t,g} - {g,k,k,k,m,l} ) - ( {k,g,a,a,a} - {k,l,m} )")
    }
  }

  test("The MultiSetRepl should evaluate intersections") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{a}") {
      repl.readEval("{a,a} * {a}")
    }

    assertResult("{a,b}") {
      repl.readEval("{a,a,b,c,c,e} * {a,b,f,f,g}")
    }

    assertResult("{a}") {
      repl.readEval("{a,b,c} * {a,a,f,f} * {f,f,f,r,t,a} * {f,n,a}")
    }

    assertResult("{g}") {
      repl.readEval("{a,g,c} * ( {d,e,g,g} * {g,g,h,d} )")
    }

    assertResult("{r}") {
      repl.readEval("( {g,g,r,r,t} * {r,t,e,d} ) * ( {y,y,u,d,r} * {y,u,r,d} )")
    }
  }

  test("The MultiSetRepl should evaluate expressions") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{a,a,b,d,d}") {
      repl.readEval("{a,a,b,d} + {r,r,t,d} * {i,j,k,d}")
    }

    assertResult("{r,r,s,v,x,x}") {
      repl.readEval("{s,x,x,v} * {z,f,x,x,x,v} + {r,r,s}")
    }

    assertResult("{f,f,h,h,j,r}") {
      repl.readEval("{j,h,h,f} + {f,r,r,t} * {e,e,r,f}")
    }

    assertResult("{q,v,w,w}") {
      repl.readEval("{w,w,q,v} * ( {w,w,q,v} + {l,l,n} )")
    }

    assertResult("{c,f,x,x,z}") {
      repl.readEval("( {c,c,x,z} - {c,z,s} ) + {f,x,z}")
    }

    assertResult("{r,r,u,w,y}") {
      repl.readEval("{r,w,w,y} * ( {r,r,t} + {q,q,w} * {t,t,w} ) + {y,u,r}")
    }

    assertResult("{f,h,t,t,y}") {
      repl.readEval("( ( {h,h,r} - {h,r,f,g} * {h,r,e,r} ) + ( {t,t,t,y,f} - ( {t,t,y} ) * {t,e,e,r} ) )")
    }

    assertResult("{e,f}") {
      repl.readEval(
        "{e} + ( {f,f,g,t} * ( ( {f,f,r,t} - {a,a,n,d} ) + ( {f,g} ) - {r,t,d,d} ) - ( {f,t,y} + {y,y,r} ) ) * {f,y,y,e}"
      )
    }
  }

  test("The MultiSetRepl should be able to assign and use variables") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("n = {a}") {
      repl.readEval("n = {a}")
    }

    assertResult("n = {a,b,b,n}") {
      repl.readEval("n = {a,b,n} + {b,n,n} * {r,r,t,b}")
    }

    assertResult("m = {a,b,b,b,c,m}") {
      repl.readEval("m = {a,b,c}")
      repl.readEval("m = m + {b,b,m}")
    }

    assertResult("p = {g,g,g,g,h,n,y}") {
      repl.readEval("p = {g,g,y}")
      repl.readEval("p = ( {g,h,h,n} + p * {g,g,g,y,y,i} ) * {g,g,h,n,p} + p")
    }

    assertResult("{n,r,r,t,y,y}") {
      repl.readEval("q = {y,t,r}")
      repl.readEval("{r,y,n} + q")
    }

    assertResult("{g,g}") {
      repl.readEval("r = {g,g,g,r,r,c} * {c,c,c,g,g,d,d,e}")
      repl.readEval("r = ( r * {g,c,c} - {d,d,d,c} ) + {g,h,y}")
      repl.readEval("{} + ( r * r ) - ( {h,n,m} + {y,m,m,r} )")
    }
  }

  test("The MultiSetRepl should be able to simplify (and pretty print) expressions") {
    val repl = REPLFactory.makeMultiSetREPL()

    assertResult("{a}") {
      repl.readEval("@ {a}")
    }

    assertResult("{a} + {b}") {
      repl.readEval("@ {a} + {b}")
    }

    assertResult("{a}") {
      repl.readEval("@ {a} + {}")
    }

    assertResult("{a}") {
      repl.readEval("@ {} + {a}")
    }

    assertResult("{}") {
      repl.readEval("@ {a} * {}")
    }

    assertResult("{}") {
      repl.readEval("@ {} * {a}")
    }

    assertResult("{a}") {
      repl.readEval("@ {a} * {a}")
    }

    assertResult("{a,b,c} * ({b,c,d} + {e,f,g})") {
      repl.readEval("@ ( {a,b,c} * {b,c,d} ) + ( {a,b,c} * {e,f,g} )")
    }

    assertResult("{b,b,b} * ({g,g,h} + {h,h,i,j})") {
      repl.readEval("@ ( {g,g,h} * {b,b,b} ) + ( {b,b,b} * {h,h,j,i} )")
    }

    assertResult("{b} * ({j,j,j} + {f,g,j})") {
      repl.readEval("@ ( {b} * {j,j,j} ) + ( {f,g,j} * {b} )")
    }

    assertResult("{h,j} * ({f,g,g} + {g,h,h,i})") {
      repl.readEval("@ ( {f,g,g} * {h,j} ) + ( {g,h,h,i} * {h,j} )")
    }

    assertResult("{b,b,m,n} * {b,c,v} + {b,h,n} * {g,u,y}") {
      repl.readEval("@ ( {b,b,n,m} * {c,v,b} ) + ( {b,n,h} * {g,y,u} )")
    }
  }
}
