/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace RelLib/Tree {

    use FlixSandbox.RoseTree;
    use FlixSandbox.RoseTree.{Node};


    /// Tree2

    pub enum Level2[a, b] { 
        case RootOf2
        case Level1Of2(a)
        case Level2Of2(b)
    }

    instance ToString[Level2[a, b]] with ToString[a], ToString[b] {
        pub def toString(x: Level2[a, b]): String = match x {
            case RootOf2 => "RootOf2"
            case Level1Of2(a) => "Level1Of2(${a})"
            case Level2Of2(b) => "Level2Of2(${b})"
        }
    }


    
    def genKids2(node1: (a, List[b]) -> RoseTree[c] & ef, groups: Map[a, List[b]]): List[RoseTree[c]] & ef with Order[a]  = 
        let dskids = Map.foldLeftWithKey((ds, k, v) -> Chain.snoc(ds, node1(k, v)), Chain.empty(), groups);
        Chain.toList(dskids)

    
    /// We want this exposed by the API to support printing to HTML, maybe JSON etc.
    pub def tree2(arr: Array[(a, b), r]): RoseTree[Level2[a, b]] \ Read(r) with Order[a]  =
        let m1 = RelLib/Organization.intoMapList(fst, snd, arr); 
        Node(RootOf2, genKids2((x1, kids) -> Node(Level1Of2(x1), List.map(x2 -> Node(Level2Of2(x2), Nil), kids)), m1))

    pub def drawTree2(rootLabel: String, tree: RoseTree[Level2[a, b]]): String with ToString[a], ToString[b] = 
        let nodeToString = x1 -> match x1 { 
            case RootOf2 => rootLabel
            case Level1Of2(a) => ToString.toString(a)
            case Level2Of2(b) => ToString.toString(b)
        };
        FlixSandbox/RoseTree.map(nodeToString, tree)
            |> FlixSandbox/RoseTree.drawTree

    pub def printAsTree2(arr: Array[(a, b), r]): Unit \ {Impure, Read(r)} with Order[a], ToString[a], ToString[b] = 
        tree2(arr) 
            |> drawTree2("Root") 
            |> println


    /// Tree3

    pub enum Level3[a, b, c] { 
        case RootOf3
        case Level1Of3(a)
        case Level2Of3(b)
        case Level3Of3(c)
    }

    instance ToString[Level3[a, b, c]] with ToString[a], ToString[b], ToString[c] {
        pub def toString(x: Level3[a, b, c]): String = match x {
            case RootOf3 => "RootOf3"
            case Level1Of3(a) => "Level1Of3(${a})"
            case Level2Of3(b) => "Level2Of3(${b})"
            case Level3Of3(c) => "Level3Of3(${c})"
        }
    }

    pub def tree3(arr: Array[(a, b, c), r]): RoseTree[Level3[a, b, c]] \ Read(r) with Order[a], Order[b]  = 
        region r1 {
            let getKey  = RelLib/Tuple.decons3((x1, _, _) -> x1);
            let getBody = RelLib/Tuple.decons3((_, x2, x3) -> (x2, x3));
            let groups  = RelLib/Organization.intoMapList(getKey, getBody, arr);
            let kids    = genKids2((x1, ks) -> Node(Level1Of3(x1), tree3GenLevel2(List.toArray(ks, r1))), groups);
            Node(RootOf3, kids)
        }

    def tree3GenLevel2(arr: Array[(b, c), r]): List[RoseTree[Level3[a, b, c]]] \ Read(r) with Order[b] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level2Of3(x1), List.map(x2 -> Node(Level3Of3(x2), Nil), kids)), groups)

    pub def drawTree3(rootLabel: String, tree: RoseTree[Level3[a, b, c]]): String with ToString[a], ToString[b], ToString[c] = 
        let nodeToString = x1 -> match x1 { 
            case RootOf3 => rootLabel
            case Level1Of3(a) => ToString.toString(a)
            case Level2Of3(b) => ToString.toString(b)
            case Level3Of3(c) => ToString.toString(c)
        };
        FlixSandbox/RoseTree.map(nodeToString, tree)
            |> FlixSandbox/RoseTree.drawTree

    pub def printAsTree3(arr: Array[(a, b, c), r]): Unit \ { Impure, Read(r) } with Order[a], Order[b], ToString[a], ToString[b], ToString[c] = 
        tree3(arr) 
            |> drawTree3("Root") 
            |> println

    /// Tree4

    pub enum Level4[a, b, c, d] { 
        case RootOf4
        case Level1Of4(a)
        case Level2Of4(b)
        case Level3Of4(c)
        case Level4Of4(d)
    }

    instance ToString[Level4[a, b, c, d]] with ToString[a], ToString[b], ToString[c], ToString[d] {
        pub def toString(x: Level4[a, b, c, d]): String = match x {
            case RootOf4 => "RootOf4"
            case Level1Of4(a) => "Level1Of4(${a})"
            case Level2Of4(b) => "Level2Of4(${b})"
            case Level3Of4(c) => "Level3Of4(${c})"
            case Level4Of4(d) => "Level4Of4(${d})"
        }
    }

    pub def tree4(arr: Array[(a, b, c, d), r]): RoseTree[Level4[a, b, c, d]] \ Read(r) with Order[a], Order[b], Order[c]  = 
        region r1 { 
            let getKey  = RelLib/Tuple.decons4((x1, _, _, _) -> x1);
            let getBody = RelLib/Tuple.decons4((_, x2, x3, x4) -> (x2, (x3, x4)));
            let groups  = RelLib/Organization.intoMapList(getKey, getBody, arr);
            let kids    = genKids2((x1, ks) -> Node(Level1Of4(x1), tree4GenLevel2(List.toArray(ks, r1), r1)), groups);
            Node(RootOf4, kids)
        }

    def tree4GenLevel2(arr: Array[(b, (c, d)), r1], r: Region[r2]): List[RoseTree[Level4[a, b, c, d]]] \ { Read(r1), Write(r2) } with Order[b], Order[c] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level2Of4(x1), tree4GenLevel3(List.toArray(kids, r))), groups)

    def tree4GenLevel3(arr: Array[(c, d), r]): List[RoseTree[Level4[a, b, c, d]]] \ Read(r) with  Order[c] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level3Of4(x1), List.map(x2 -> Node(Level4Of4(x2), Nil), kids)), groups)

    pub def drawTree4(rootLabel: String, tree: RoseTree[Level4[a, b, c, d]]): String with ToString[a], ToString[b], ToString[c], ToString[d] = 
        let nodeToString = x1 -> match x1 { 
            case RootOf4 => rootLabel
            case Level1Of4(a) => ToString.toString(a)
            case Level2Of4(b) => ToString.toString(b)
            case Level3Of4(c) => ToString.toString(c)
            case Level4Of4(d) => ToString.toString(d)
        };
        FlixSandbox/RoseTree.map(nodeToString, tree)
            |> FlixSandbox/RoseTree.drawTree

    pub def printAsTree4(arr: Array[(a, b, c, d), r]): Unit \ { Impure, Read(r) } with Order[a], Order[b], Order[c], ToString[a], ToString[b], ToString[c], ToString[d] = 
        tree4(arr) 
            |> drawTree4("Root") 
            |> println

    /// Tree5

    pub enum Level5[a, b, c, d, e] { 
        case RootOf5
        case Level1Of5(a)
        case Level2Of5(b)
        case Level3Of5(c)
        case Level4Of5(d)
        case Level5Of5(e)
    }

    instance ToString[Level5[a, b, c, d, e]] with ToString[a], ToString[b], ToString[c], ToString[d], ToString[e] {
        pub def toString(x: Level5[a, b, c, d, e]): String = match x {
            case RootOf5 => "RootOf5"
            case Level1Of5(a) => "Level1Of5(${a})"
            case Level2Of5(b) => "Level2Of5(${b})"
            case Level3Of5(c) => "Level3Of5(${c})"
            case Level4Of5(d) => "Level4Of5(${d})"
            case Level5Of5(e) => "Level5Of5(${e})"
        }
    }


    pub def tree5(arr: Array[(a, b, c, d, e), r]): RoseTree[Level5[a, b, c, d, e]] \ Read(r) with Order[a], Order[b], Order[c], Order[d] = 
        region r1 {
            let getKey  = RelLib/Tuple.decons5((x1, _, _, _, _) -> x1);
            let getBody = RelLib/Tuple.decons5((_, x2, x3, x4, x5) -> (x2, (x3, (x4, x5))));
            let groups  = RelLib/Organization.intoMapList(getKey, getBody, arr);
            let kids    = genKids2((x1, ks) -> Node(Level1Of5(x1), tree5GenLevel2(List.toArray(ks, r1), r1)), groups);
            Node(RootOf5, kids)
        }

    def tree5GenLevel2(arr: Array[(b, (c, (d, e))), r1], r: Region[r2]): List[RoseTree[Level5[a, b, c, d, e]]] \ { Read(r1), Write(r2) } with Order[b], Order[c], Order[d] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level2Of5(x1), tree5GenLevel3(List.toArray(kids, r), r)), groups)

    def tree5GenLevel3(arr: Array[(c, (d, e)), r1], r: Region[r2]): List[RoseTree[Level5[a, b, c, d, e]]] \ { Read(r1), Write(r2) } with Order[c], Order[d] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level3Of5(x1), tree5GenLevel4(List.toArray(kids, r))), groups)

    def tree5GenLevel4(arr: Array[(d, e), r]): List[RoseTree[Level5[a, b, c, d, e]]] \ Read(r) with Order[d] = 
        let groups = RelLib/Organization.intoMapList(fst, snd, arr); 
        genKids2((x1, kids) -> Node(Level4Of5(x1), List.map(x2 -> Node(Level5Of5(x2), Nil), kids)), groups)


    pub def drawTree5(rootLabel: String, tree: RoseTree[Level5[a, b, c, d, e]]): String with ToString[a], ToString[b], ToString[c], ToString[d], ToString[e] = 
        let nodeToString = x1 -> match x1 { 
            case RootOf5 => rootLabel
            case Level1Of5(a) => ToString.toString(a)
            case Level2Of5(b) => ToString.toString(b)
            case Level3Of5(c) => ToString.toString(c)
            case Level4Of5(d) => ToString.toString(d)
            case Level5Of5(e) => ToString.toString(e)
        };
        FlixSandbox/RoseTree.map(nodeToString, tree)
            |> FlixSandbox/RoseTree.drawTree


    pub def printAsTree5(arr: Array[(a, b, c, d, e), r]): Unit \ {Impure, Read(r)} with Order[a], Order[b], Order[c], Order[d], ToString[a], ToString[b], ToString[c], ToString[d], ToString[e] =  
        tree5(arr) 
            |> drawTree5("Root") 
            |> println

}
    

