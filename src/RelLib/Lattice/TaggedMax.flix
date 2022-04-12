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

/// TODO can Tagged be a lattice transformer?

namespace RelLib/Lattice/TaggedMax {

    use RelLib/Lattice/Ordered.{Ordered};
    use RelLib/Lattice/Ordered.Ordered.{Ordered};

    pub enum TaggedMax[u, a] with Eq, Order, ToString, Boxable { 
        case Top
        case TaggedMax(u, a) 
        case Bottom
    }

    pub def taggedMax(tag: u, x: a): TaggedMax[u, a] = TaggedMax(tag, x)

    instance Functor[TaggedMax[u]] {
        pub def map(f: a -> b & ef, x: TaggedMax[u, a]): TaggedMax[u, b] & ef = match x {
            case Top => Top as & ef
            case TaggedMax(u, a) => TaggedMax(u, f(a))
            case Bottom => Bottom as & ef
        }
    }

    instance PartialOrder[TaggedMax[u, a]] with Order[a] {
        pub def lessEqual(x: TaggedMax[u, a], y: TaggedMax[u, a]): Bool = match (x, y) {
            case (Bottom, _) => true
            case (Top, Bottom) => false
            case (Top, TaggedMax(_, _)) => false
            case (Top, Top) => true
            case (TaggedMax(_, _), Bottom) => false
            case (TaggedMax(_, _), Top) => true
            case (TaggedMax(_, x1), TaggedMax(_, y1)) => x1 <= y1
        }
    }

    instance LowerBound[TaggedMax[u, a]] {
        pub def minValue(): TaggedMax[u, a] = Bottom
    }

    instance UpperBound[TaggedMax[u, a]] {
        pub def maxValue(): TaggedMax[u, a] = Top
    }


    instance JoinLattice[TaggedMax[u, a]] with Order[a] {
        /// \/
        pub def leastUpperBound(x: TaggedMax[u, a], y: TaggedMax[u, a]): TaggedMax[u, a] = match (x, y) {
            case (Top, _) => Top
            case (Bottom, y1) => y1
            case (TaggedMax(_, _), Top) => Top
            case (TaggedMax(u1, x1), Bottom) => TaggedMax(u1, x1)
            case (TaggedMax(u1, x1), TaggedMax(v1, y1)) => if (x1 <= y1) TaggedMax(v1, y1) else TaggedMax(u1, x1)
        }
    }

    instance MeetLattice[TaggedMax[u, a]] with Order[a] {
        /// /\
        pub def greatestLowerBound(x: TaggedMax[u, a], y: TaggedMax[u, a]): TaggedMax[u, a] = 
            match (x, y) {
                case (Bottom, _) => Bottom
                case (Top, y1) => y1
                case (TaggedMax(_, _), Bottom) => Bottom
                case (TaggedMax(u1, x1), Top) => TaggedMax(u1, x1)    
                case (TaggedMax(u1, x1), TaggedMax(_, y1)) => if (x1 <= y1) TaggedMax(u1, x1) else Bottom
            }
    }

}
