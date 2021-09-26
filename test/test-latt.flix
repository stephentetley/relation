use RelLib/Lattice/Min.{min1};
use RelLib/Lattice/Max.{max1};
use RelLib/Lattice/All.All.{All};
use RelLib/Lattice/Any.Any.{Any};
use RelLib/Lattice/Addition.Addition.{Addition};

def main(_args: Array[String]) : Int32 & Impure = 
    Console.printLine("Running...");
    getMin();
    getMax();
    getAll();
    getAny();
    getAddition();
    getTaggedMax();
    0

def getMin(): Unit & Impure = 
    let p = #{
        
        Calls("Anna", "Brian"; min1(4)).
        Calls("Anna", "Brian"; min1(10)).
        Calls("Brian", "Colin"; min1(5)).
        
    };
    Console.printLine("getMin...");
    query p select (x1, x2; x3) from Calls(x1, x2; x3) 
        |> Array.foreach(println)

def getMax(): Unit & Impure = 
    let p = #{
        
        Calls("Anna", "Brian"; max1(4)).
        Calls("Anna", "Brian"; max1(10)).
        Calls("Brian", "Colin"; max1(5)).
        
    };
    Console.printLine("getMax...");
    query p select (x1, x2; x3) from Calls(x1, x2; x3) 
        |> Array.foreach(println)


def getAll(): Unit & Impure = 
    let p = #{
        
        Calls("Anna", "Brian", 4).
        Calls("Anna", "Brian", 10).
        Calls("Brian", "Colin", 5).
        Calls("Colin", "Delia", 15).

        LT10(a, b; All(dur < 10)) :- Calls(a, b, dur).
        
    };
    Console.printLine("getAll...");
    query p select (x1, x2; x3) from LT10(x1, x2; x3) 
        |> Array.foreach(println)

def getAny(): Unit & Impure = 
    let p = #{
        
        Calls("Anna", "Brian", 4).
        Calls("Anna", "Brian", 10).
        Calls("Brian", "Colin", 5).
        Calls("Colin", "Delia", 15).

        LT10(a, b; Any(dur < 10)) :- Calls(a, b, dur).
        
    };
    Console.printLine("getAny...");
    query p select (x1, x2; x3) from LT10(x1, x2; x3) 
        |> Array.foreach(println)


def getAddition(): Unit & Impure = 
    let p = #{
        
        Calls("Anna", "Brian", 4).
        Calls("Anna", "Brian", 10).
        Calls("Anna", "Brian", 2).
        Calls("Brian", "Colin", 5).
        Calls("Brian", "Colin", 3).
        Calls("Colin", "Delia", 15).

        Add(a, b; Addition(dur)) :- Calls(a, b, dur).
        
    };
    Console.printLine("getAddition...");
    query p select (x1, x2; x3) from Add(x1, x2; x3) 
        |> Array.foreach(println)


def getTaggedMax(): Unit & Impure = 
    use RelLib/Lattice/TaggedMax.{taggedMax};
    let p = #{
        
        Calls("Anna", "Brian"; taggedMax("four", 4)).
        Calls("Anna", "Brian"; taggedMax("ten", 10)).
        Calls("Brian", "Colin"; taggedMax("five", 5)).
        
    };
    Console.printLine("getTaggedMax...");
    query p select (x1, x2; x3) from Calls(x1, x2; x3) 
        |> Array.foreach(println)        