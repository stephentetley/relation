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

namespace Relation/Tree {

    use Data.RoseTree;
    use Data.RoseTree.{RoseTree};

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


    
    def genKids2(node1: a -> c & ef1, node2: b -> c & ef2, groups: Map[a, List[b]]): List[RoseTree[c]] & Impure with Order[a]  = 
        let makeNode2 = x1 -> RoseTree(node2(x1) as & Impure, Nil);
        let makeNode1 = (x1, xs) -> RoseTree(node1(x1) as & Impure, List.map(makeNode2, xs));
        let dskids = Map.foldLeftWithKey((ds, k, v) -> Data/DList.snoc(ds, makeNode1(k, v)), Data/DList.empty(), groups);
        Data/DList.toList(dskids)

    
    /// We want this exposed by the API to support printing to HTML, maybe JSON etc.
    pub def tree2(arr: Array[(a, b)]): RoseTree[Level2[a, b]] & Impure with Order[a]  =
        let m1 = Relation/Organization.collectToMapList(fst, snd, arr); 
        RoseTree(RootOf2, genKids2(Level1Of2, Level2Of2, m1))

    pub def drawTree2(rootLabel: String, tree: RoseTree[Level2[a, b]]): String with ToString[a], ToString[b] = 
        let nodeToString = x1 -> match x1 { 
            case RootOf2 => rootLabel
            case Level1Of2(a) => ToString.toString(a)
            case Level2Of2(b) => ToString.toString(b)
        };
        Data/RoseTree.map(nodeToString, tree)
            |> Data/RoseTree.drawTree

    pub def printAsTree2(arr: Array[(a, b)]): Unit & Impure with Order[a], ToString[a], ToString[b] = 
        tree2(arr) 
            |> drawTree2("Root") 
            |> println


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

    /// pub def tree3(arr: Array[(a, b, c)]): RoseTree[Level3[a, b, c]] & Impure with Order[a], Order[b]  = 
    ///     use Relation/Tuple.{decons3};
    ///     let getKey  = Array.map(decons3((x1, _, _) -> x1);
    ///     let getBody = Array.map(decons3((_, x2, x3) -> (x2, x3));
    ///     let groups = Relation/Organization.collectToMapList(getKey, snd, getBody);
    ///     let kids = genKids2(Level1Of3, , groups);
    ///     RoseTree(RootOf3, kids)

    /// def tree3GenLevel2(arr: Array[(b, c)]): List[RoseTree[Level3[a, b, c]]] & Impure with Order[b] = 
    ///     let groups = Relation/Organization.collectToMapList(fst, snd, arr); 
    ///     genKids2(Level2Of3, Level3Of3, groups)


}
    

