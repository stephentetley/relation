use Data.RoseTree;
use Data.RoseTree.{RoseTree};

use RelLib/UnboundTree.{PreorderRow};


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01() |> println;
    0

def test01(): Result[Unit, String] & Impure = 
    use Result.flatMap;
    let _ = RelLib/UnboundTree.printAsTree("", rows());
    Ok()

def rows(): Array[PreorderRow[String, String]] & Impure = 
    let r1 = ("WETHE",                  "WETHE (Site)",                     "");
    let r2 = ("WETHE-CAA",              "WETHE-CAA (Fun)",                  "WETHE");
    let r3 = ("WETHE-CAA-CON",          "WETHE-CAA-CON (Proc)",             "WETHE-CAA");
    let r4 = ("WETHE-CAA-CON-LOC",      "WETHE-CAA-CON-LOC (Proc_Group)",   "WETHE-CAA-CON");
    let r5 = ("WETHE-CAA-NET",          "WETHE-CAA-NET (Proc)",             "WETHE-CAA");
    let r6 = ("WETHE-CAA-NET-TEL",      "WETHE-CAA-NET-TEL (Proc_Group)",   "WETHE-CAA-NET");
    let r7 = ("WETHE-E",                "WETHE-E (Fun)",                    "WETHE");    
    let r8 = ("YORK1",                  "YORK1 (Site)",                     "");    
    let r9 = ("YORK1-CAA",              "YORK1-CAA (Fun)",                  "YORK1");
    [r1, r2, r3, r4, r5, r6, r7, r8, r9]


