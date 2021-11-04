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

namespace RelLib/UnboundTree {

    use Data.RoseTree;
    use Data.RoseForest;
    use Data.RoseTree.{RoseTree};


    pub type alias PreorderRow[k, v] = (k, v, k)

    /// `rootKey` is the parent key of the topmost element(s), it should not be present as an item key.
    pub def unboundForest(rootKey: k, rows: Array[PreorderRow[k, v]]): RoseForest[v] & Impure with Eq[k] =  
        treesList(rootKey, Array.toList(rows), (xs, _) -> xs) 


    /// This is preorder builder with unlimited depth - input *must* be correctly ordered.
    def treesList(parentId: k, xs: List[PreorderRow[k, v]], k: (List[RoseTree[v]], List[PreorderRow[k, v]]) -> List[RoseTree[v]]): List[RoseTree[v]] with Eq[k] =
        match xs { 
            case r1 :: rest => {
                let (key, label, parentKey) = r1;
                if (parentId == parentKey)
                    treesList(key, rest, (kids, rest1) -> {
                        let node = RoseTree(label, kids);
                        treesList(parentId, rest1, (siblings, rest2) -> k(node :: siblings, rest2))
                    })
                else
                    k(Nil, xs)
            }
            case Nil => k(Nil, Nil)
        }

    /// Note - we build a forest, so synthesize a root node unless we have a singleton.
    pub def printAsTree(rootKey: k, arr: Array[PreorderRow[k, v]]): Unit & Impure with Eq[k], ToString[v] = 
        match unboundForest(rootKey, arr) { 
            case t1 :: Nil => Data/RoseTree.map(ToString.toString, t1) |> Data/RoseTree.drawTree |> println
            case trees => 
                RoseTree("Root", List.map(Data/RoseTree.map(ToString.toString), trees))
                    |> Data/RoseTree.drawTree
                    |> println
        }

}
