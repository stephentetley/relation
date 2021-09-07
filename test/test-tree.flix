use Data.RoseTree;
use Data.RoseTree.{RoseTree};

use Relation/Tree.Level3;
use Relation/Tree.Level3.{RootOf3, Level1Of3, Level2Of3, Level3Of3};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01();
    Data/RoseTree.map(ToString.toString, t1()) |> Data/RoseTree.drawTree |> println;
    [("A", "B1", "X"), ("A", "B2", "Y")] |> Relation/Tree.printAsTree3;
    0


pub def t1(): RoseTree[Level3[String, Char, Int32]] = 
    RoseTree(RootOf3, RoseTree(Level1Of3("hello"), RoseTree(Level2Of3('X'), RoseTree(Level3Of3(1), Nil) :: Nil) :: Nil) ::Nil)


rel Organization(set: String, pump: String)
rel Pump(name: String, pumpType: String)

rel PumpSet(name: String, set: String)
rel PumpId(name: String, ident: String)

def test01(): Unit & Impure = 
    query rules() select (x1, x2) from Organization(x1, x2)
        |> Relation/Tree.printAsTree2


def rules(): #{Organization, Pump, PumpSet, PumpId |r} =  #{

    Organization(set, pumpId) :- 
        Pump(name, _) ,
        PumpSet(name, set),
        PumpId(name, pumpId).

    Pump("Booster Set 1 Pump-1", "Centrifugal Pump").
    Pump("Booster Set 1 Pump-2", "Centrifugal Pump").
    Pump("Backflush Pump", "Centrifugal Pump").
    Pump("Final Effluent Pump-1", "Submersible Pump").
    Pump("Final Effluent Pump-2", "Submersible Pump").
    Pump("Booster Set 2 Pump-1", "Centrifugal Pump").
    Pump("Booster Set 2 Pump-2", "Centrifugal Pump").

    PumpSet("Booster Set 1 Pump-1", "Booster Set 1").
    PumpSet("Booster Set 1 Pump-2", "Booster Set 1").
    PumpSet("Backflush Pump", "Backflush").
    PumpSet("Final Effluent Pump-1", "Final Effluent").
    PumpSet("Final Effluent Pump-2", "Final Effluent").
    PumpSet("Booster Set 2 Pump-1", "Booster Set 2").
    PumpSet("Booster Set 2 Pump-2", "Booster Set 2").

    PumpId("Booster Set 1 Pump-1", "Pump-1").
    PumpId("Booster Set 1 Pump-2", "Pump-2").
    PumpId("Backflush Pump", "Backflush Pump").
    PumpId("Final Effluent Pump-1", "Pump-1").
    PumpId("Final Effluent Pump-2", "Pump-2").
    PumpId("Booster Set 2 Pump-1", "Pump-1").
    PumpId("Booster Set 2 Pump-2", "Pump-2").

}
