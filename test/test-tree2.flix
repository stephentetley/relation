
namespace TestTree2 {

    use Data.RoseTree;
    use Data.RoseTree.{RoseTree};

    use RelLib/Tree2.{Path};


    @test
    def test01(): Result[Unit, String] & Impure = 
        use Result.flatMap;
        paths() |> List.foreach(println);
        let* tree1 = paths() |> List.toArray |> RelLib/Tree2.buildTree;
        Data/RoseTree.drawTree(tree1) |> println;
        Ok()

    def paths(): List[Path] = 
        let p1 = RelLib/Tree2.path4(("WETHE", "", "", ""));
        let p2 = RelLib/Tree2.path4(("WETHE", "WETHE-CAA", "", ""));
        let p3 = RelLib/Tree2.path4(("WETHE", "WETHE-CAA", "WETHE-CAA-CON", ""));
        let p4 = RelLib/Tree2.path4(("WETHE", "WETHE-CAA", "WETHE-CAA-CON", "WETHE-CAA-CON-LOC"));
        let p5 = RelLib/Tree2.path4(("WETHE", "WETHE-CAA", "WETHE-CAA-NET", ""));
        let p6 = RelLib/Tree2.path4(("WETHE", "WETHE-CAA", "WETHE-CAA-NET", "WETHE-CAA-NET-TEL"));
        let p7 = RelLib/Tree2.path4(("WETHE", "WETHE-E", "", ""));
        p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 ::Nil


}