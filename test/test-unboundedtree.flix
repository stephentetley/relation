
namespace TestUnboundTree {

    use Data.RoseTree;
    use Data.RoseTree.{RoseTree};

    use RelLib/UnboundedTree.{PreorderRow};


    @test
    def test01(): Result[Unit, String] & Impure = 
        use Result.flatMap;
        region r {
            let extractor = 
                { getKey = RelLib/Tuple.decons3((x, _, _) -> x)
                , getParent = RelLib/Tuple.decons3((_, _, z) -> z)
                , getValue = RelLib/Tuple.decons3((_, y, _) -> y)
                };
            let _ = RelLib/UnboundedTree.printAsTree("", extractor, rows(r));
            Ok()
        }
        
    def rows(r: Region[r]): Array[PreorderRow[String, String], r] \ Write(r) = 
        let r1 = ("WETHE",                  "WETHE (Site)",                     "");
        let r2 = ("WETHE-CAA",              "WETHE-CAA (Fun)",                  "WETHE");
        let r3 = ("WETHE-CAA-CON",          "WETHE-CAA-CON (Proc)",             "WETHE-CAA");
        let r4 = ("WETHE-CAA-CON-LOC",      "WETHE-CAA-CON-LOC (Proc_Group)",   "WETHE-CAA-CON");
        let r5 = ("WETHE-CAA-NET",          "WETHE-CAA-NET (Proc)",             "WETHE-CAA");
        let r6 = ("WETHE-CAA-NET-TEL",      "WETHE-CAA-NET-TEL (Proc_Group)",   "WETHE-CAA-NET");
        let r7 = ("WETHE-E",                "WETHE-E (Fun)",                    "WETHE");    
        let r8 = ("YORK1",                  "YORK1 (Site)",                     "");    
        let r9 = ("YORK1-CAA",              "YORK1-CAA (Fun)",                  "YORK1");
        [r1, r2, r3, r4, r5, r6, r7, r8, r9] @ r


}