/*
 * Copyright 2022 Stephen Tetley
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


namespace RelLib/Lattice/Attributes {

    
    pub enum Attributes[k, v] with Eq, Order, ToString { 
        case Top
        case Attributes(Map[k, v])
        case Bottom
    }

    pub def getAttributes(x: Attributes[k, v]): Option[Map[k, v]] = match x {
        case Top           => None
        case Attributes(m) => Some(m)
        case Bottom        => None
    }

    pub def getAttributes1(x: Attributes[k, v]): Map[k, v] = match x {
        case Top           => Map.empty()
        case Attributes(m) => m
        case Bottom        => Map.empty()
    }


    pub def singleton(k: k, v: v): Attributes[k, v] with Order[k] = 
        Attributes(Map.singleton(k, v))

    instance LowerBound[Attributes[k, v]] {
        pub def minValue(): Attributes[k, v] = Bottom
    }

    instance UpperBound[Attributes[k, v]] {
        pub def maxValue(): Attributes[k, v] = Top
    }

    instance PartialOrder[Attributes[k, v]] with Order[k], Order[v] {
        pub def lessEqual(x: Attributes[k, v], y: Attributes[k, v]): Bool = match (x, y) {
            case (Bottom, _)                      => true
            case (Attributes(m1), Attributes(m2)) => m1 <= m2
            case (_, Top)                         => true
            case _                                => false
        }
    }

    /// Uses left-biased union
    instance JoinLattice[Attributes[k, v]] with Order[k], Order[v] {
        /// \/ Union
        pub def leastUpperBound(x: Attributes[k, v], y: Attributes[k, v]): Attributes[k, v] = match (x, y) {
            case (Bottom, _)                      => y
            case (_, Bottom)                      => x
            case (Attributes(m1), Attributes(m2)) => Attributes(Map.union(m1, m2))
            case _                                => Top
        }
    }

    /// Uses left-biased union
    instance MeetLattice[Attributes[k, v]] with Order[k], Order[v] {
        /// /\ Intersection
        pub def greatestLowerBound(x: Attributes[k, v], y: Attributes[k, v]): Attributes[k, v] = match (x, y) {
            case (Top, _)                         => y
            case (_, Top)                         => x
            case (Attributes(m1), Attributes(m2)) => Attributes(Map.intersection(m1, m2))
            case _                                => Bottom
        }
    }

}
