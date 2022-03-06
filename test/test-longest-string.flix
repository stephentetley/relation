
namespace TestLongestString {

    use RelLib/Lattice/LongestString.{LongestString};

    @test
    def test01(): Int32 & Impure = 
        Console.printLine("Running...");
        getPrefix();
        0

    def prefix(dir1: String, dir2: String): LongestString = 
        use RelLib/Lattice/LongestString.{longestString};
        if (String.length(dir1) <= String.length(dir2)) 
            LowerBound.minValue() 
        else {
            let prefix = String.commonPrefix(dir1, dir2);
            longestString(prefix)
        } 
            

    lat Prefix(dir: String, prefix: LongestString)

    def getPrefix(): Unit & Impure = 

        let p = #{
            
            Path("/").
            Path("/home").
            Path("/home/stephen").
            Path("/home/stephen/local.cache").
            Path("/usr").
            Path("/usr/local").
            
            Prefix(dir1; prefix(dir1, dir2)) :- 
                Path(dir1),
                Path(dir2).

            /// ParentOf(path; ancestor) :- Prefix(path; ancestor).

        };
        Console.printLine("getPrefix...");
        query p select (x1, x2) from Prefix(x1; x2) |> Array.foreach(println);
        ()

}
