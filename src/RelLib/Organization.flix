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


namespace RelLib/Organization {
    
    /// pub def groupBy(f: (a, a) -> Bool, a: Array[a]): Array[Array[a]] & Impure =

    /// Fix to Array or generalize to Foldable?

    pub def intoMap(f: a -> k & ef, g: a -> b & ef, arr: Array[a]): Map[k, b] & Impure with Order[k] = 
        let step = (x1, ac) -> {
            let k = f(x1) as & Impure;
            Map.insert(k, g(x1) as & Impure, ac)
        };        
        Array.foldRight(step, Map.empty(), arr)

    pub def intoMapList(f: a -> k & ef, g: a -> b & ef, arr: Array[a]): Map[k, List[b]] & Impure with Order[k] = 
        intoMapWith(f, Nil, (x1, xs) -> {let v1 = g(x1); v1 :: xs}, arr)

    pub def intoMapArray(f: a -> k & ef, g: a -> b & ef, arr: Array[a]): Map[k, Array[b]] & Impure with Order[k] = 
        intoMapWith(f, Nil, (x1, xs) -> {let v1 = g(x1); v1 :: xs}, arr)
            |> Map.map(flip(List.toArray, static))

    pub def intoMapSet(f: a -> k & ef, g: a -> b & ef, arr: Array[a]): Map[k, Set[b]] & Impure with Order[k], Order[b] = 
        intoMapWith(f, Set.empty(), (x1, s) -> {let v1 = g(x1); Set.insert(v1, s)}, arr)

    pub def intoMapMap(f: a -> k & ef, g: a -> (k1, v1) & ef, arr: Array[a]): Map[k, Map[k1, v1]] & Impure with Order[k], Order[k1] = 
        intoMapWith(f, Map.empty(), (x1, m) -> {let (k1, v1) = g(x1); Map.insert(k1, v1, m)}, arr)

    pub def intoMapWith(f: a -> k & ef, zero: v, combine: (a, v) -> v & ef, arr: Array[a]): Map[k, v] & Impure with Order[k] = 
        let step = (x1, ac) -> {
            let k = f(x1) as & Impure;
            Map.insertWith((_, ac1) -> combine(x1, ac1) as & Impure, k, combine(x1, zero) as & Impure, ac)
        };        
        Array.foldRight(step, Map.empty(), arr)

    /// Need functions to "unmap"...

    pub def outofMapList(f: (k, List[b]) -> c & ef, t: Map[k, List[b]]): List[c] & ef = 
        Map.foldRightWithKey((k, xs, ac) -> f(k, xs) :: ac, Nil, t)

    pub def outofMapArray(f: (k, Array[b]) -> c & Impure, t: Map[k, Array[b]]): List[c] & Impure = 
        Map.foldRightWithKey((k, xs, ac) -> f(k, xs) :: ac, Nil, t)

    pub def outofMapSet(f: (k, Set[b]) -> c & ef, t: Map[k, Set[b]]): List[c] & ef = 
        Map.foldRightWithKey((k, xs, ac) -> f(k, xs) :: ac, Nil, t)

    pub def outofMapMap(f: (k1, Map[k2, b]) -> c & ef, t: Map[k1, Map[k2, b]]): List[c] & ef = 
        Map.foldRightWithKey((k, xs, ac) -> f(k, xs) :: ac, Nil, t)

    pub def viaMapList(intoMap: (a -> k & ef1, a -> b & ef1), outofMap: (k, List[b]) -> c & ef2, arr: Array[a]): List[c] & Impure with Order[k] = 
        let f = fst(intoMap);
        let g = snd(intoMap);
        intoMapList(f, g, arr) |> outofMapList(outofMap)


}
