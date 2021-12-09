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

namespace RelLib/Tree2 {

    use Data.RoseTree;
    use Data.RoseTree.{Node};

    type alias Path = List[String]

    pub def path2(t: (a1, a2)): Path with ToString[a1], ToString[a2] = 
        match t {  
            case (x1, x2) => makePath(ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path3(t: (a1, a2, a3)): Path with ToString[a1], ToString[a2], ToString[a3] = 
        match t {  
            case (x1, x2, x3) => makePath(ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path4(t: (a1, a2, a3, a4)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4] = 
        match t {  
            case (x1, x2, x3, x4) => makePath(ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path5(t: (a1, a2, a3, a4, a5)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5] = 
        match t {  
            case (x1, x2, x3, x4, x5) => makePath(ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path6(t: (a1, a2, a3, a4, a5, a6)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6] = 
        match t {  
            case (x1, x2, x3, x4, x5, x6) => makePath(ToString.toString(x6) :: ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path7(t: (a1, a2, a3, a4, a5, a6, a7)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7] = 
        match t {  
            case (x1, x2, x3, x4, x5, x6, x7) => makePath(ToString.toString(x7) :: ToString.toString(x6) :: ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path8(t: (a1, a2, a3, a4, a5, a6, a7, a8)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8] = 
        match t {  
            case (x1, x2, x3, x4, x5, x6, x7, x8) => makePath(ToString.toString(x8) :: ToString.toString(x7) :: ToString.toString(x6) :: ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path9(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9] = 
        match t {  
            case (x1, x2, x3, x4, x5, x6, x7, x8, x9) => makePath(ToString.toString(x9) :: ToString.toString(x8) :: ToString.toString(x7) :: ToString.toString(x6) :: ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }

    pub def path10(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Path with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10] = 
        match t {  
            case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => makePath(ToString.toString(x10) :: ToString.toString(x9) :: ToString.toString(x8) :: ToString.toString(x7) :: ToString.toString(x6) :: ToString.toString(x5) :: ToString.toString(x4) :: ToString.toString(x3) :: ToString.toString(x2) :: ToString.toString(x1) :: Nil)
        }


    def makePath(xs: Path): Path = List.filter(s1 -> not String.isEmpty(s1), xs)


    pub def buildTree(input: Array[Path]): Result[RoseTree[String], String] & Impure = 
        let paths = Array.toList(input);
        match treesList(Nil, paths, (xs, _) -> xs) {
            case x1 :: Nil => Ok(x1)
            case _ :: _ => Err("buildTree2 - more than one tree")
            case Nil => Err("buildTree2 - no tree")
        }


    /// In CPS ...
    /// This is preorder builder with unlimited depth and each row is either Floc or Equi.
    /// We have to check links in the data whether to go up or down.
    def treesList(parent: Path, xs: List[Path], k: (List[RoseTree[String]], List[Path]) -> List[RoseTree[String]]): List[RoseTree[String]] =
        match xs { 
            case path1 :: rest => {
                if (isChildNode(parent, path1))
                    treesList(path1, rest, (kids, rest1) -> {
                        let label = top(path1);
                        let node = Node(label, kids);
                        treesList(parent, rest1, (siblings, rest2) -> k(node :: siblings, rest2))
                    })
                else
                    k(Nil, xs)
            }
            case Nil => k(Nil, Nil)
        }

    def isChildNode(parentPath: Path, childPath: Path): Bool = match childPath {
        case _ :: rest => rest == parentPath
        case Nil => false
    }

    def top(x: Path): String = match x {
        case x1 :: _ => x1
        case Nil => ""      
    }

    pub def printAsTree2(arr: Array[(a1, a2)]): Unit & Impure with ToString[a1], ToString[a2] = 
        Array.map(path2, arr) |> printAsTreeHelper

    pub def printAsTree3(arr: Array[(a1, a2, a3)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3] = 
        Array.map(path3, arr) |> printAsTreeHelper

    pub def printAsTree4(arr: Array[(a1, a2, a3, a4)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4] = 
        Array.map(path4, arr) |> printAsTreeHelper

    pub def printAsTree5(arr: Array[(a1, a2, a3, a4, a5)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5] = 
        Array.map(path5, arr) |> printAsTreeHelper

    pub def printAsTree6(arr: Array[(a1, a2, a3, a4, a5, a6)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6] = 
        Array.map(path6, arr) |> printAsTreeHelper

    pub def printAsTree7(arr: Array[(a1, a2, a3, a4, a5, a6, a7)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7] = 
        Array.map(path7, arr) |> printAsTreeHelper

    pub def printAsTree8(arr: Array[(a1, a2, a3, a4, a5, a6, a7, a8)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8] = 
        Array.map(path8, arr) |> printAsTreeHelper

    pub def printAsTree9(arr: Array[(a1, a2, a3, a4, a5, a6, a7, a8, a9)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9] = 
        Array.map(path9, arr) |> printAsTreeHelper

    pub def printAsTree10(arr: Array[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)]): Unit & Impure with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10] = 
        Array.map(path10, arr) |> printAsTreeHelper


    def printAsTreeHelper(paths: Array[Path]): Unit & Impure = 
        match (paths |> buildTree) { 
            case Err(msg) => println("Error: ${msg}")
            case Ok(tree) => Data/RoseTree.drawTree(tree) |> println
        }


}
    

