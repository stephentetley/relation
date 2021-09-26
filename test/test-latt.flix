use RelLib/Lattice/Min.{min1};
use RelLib/Lattice/Max.{max1};

def main(_args: Array[String]) : Int32 & Impure = 
    Console.printLine("Running...");
    getMin();
    getMax();
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