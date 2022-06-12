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

namespace RelLib/UnboundedTree {

    use FlixSandbox.RoseTree;
    use FlixSandbox.RoseForest;
    use FlixSandbox.RoseTree.{Node};


    pub type alias PreorderRow[k, v] = (k, v, k)


    /// `unboundForest` takes a dictionary:
    ///
    pub type alias TreeRowExtractor[row, k, v] = 
        { getKey :: row -> k 
        , getParent :: row -> k 
        , getValue :: row -> v
        }
    
    def extractKeyValueParent(extractor: TreeRowExtractor[row, k, v], r: row): (k, v, k) =
        ((extractor.getKey)(r), (extractor.getValue)(r), (extractor.getParent)(r))


    /// `rootKey` is the parent key of the topmost element(s), it should not be present as an item key.
    pub def unboundForest(rootKey: k, 
                            extractor: TreeRowExtractor[row, k, v], 
                            rows: Array[row, r]): RoseForest[v] \ Read(r) with Eq[k] =  
        treesList(rootKey, extractor, Array.toList(rows), (xs, _) -> xs) 


    /// This is preorder builder with unlimited depth - input *must* be correctly ordered.
    def treesList(parentId: k, 
                    extractor: TreeRowExtractor[row, k, v], 
                    xs: List[row], 
                    k: (List[RoseTree[v]], List[row]) -> List[RoseTree[v]]): List[RoseTree[v]] with Eq[k] =
        match xs { 
            case r1 :: rest => {
                let (key, label, parentKey) = extractKeyValueParent(extractor, r1);
                if (parentId == parentKey)
                    treesList(key, extractor, rest, (kids, rest1) -> {
                        let node = Node(label, kids);
                        treesList(parentId, extractor, rest1, (siblings, rest2) -> k(node :: siblings, rest2))
                    })
                else
                    k(Nil, xs)
            }
            case Nil => k(Nil, Nil)
        }

    /// Note - we build a forest, so synthesize a root node unless we have a singleton.
    pub def printAsTree(rootKey: k, 
                        extractor: TreeRowExtractor[row, k, v], 
                        arr: Array[row, r]): Unit \ { Impure, Read(r) } with Eq[k], ToString[v] = 
        match unboundForest(rootKey, extractor, arr) { 
            case t1 :: Nil => FlixSandbox/RoseTree.map(ToString.toString, t1) |> FlixSandbox/RoseTree.drawTree |> println
            case trees => 
                Node("Root", List.map(FlixSandbox/RoseTree.map(ToString.toString), trees))
                    |> FlixSandbox/RoseTree.drawTree
                    |> println
        }

}
