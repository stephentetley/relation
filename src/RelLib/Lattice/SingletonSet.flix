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

/// Same as Constant?

namespace RelLib/Lattice/SingletonSet {

    pub enum SingletonSet[a] with Eq, Order, ToString, Boxable { 
        case Bottom
        case Single(a)
        case Top
    }

    pub def singleton(x: a): SingletonSet[a] = Single(x)

    instance Functor[SingletonSet] {
        pub def map(f: a -> b & ef, x: SingletonSet[a]): SingletonSet[b] & ef = match x {
            case Bottom => Bottom as & ef
            case Single(a) => Single(f(a))
            case Top => Top as & ef
        }
    }

    instance PartialOrder[SingletonSet[a]] with Eq[a] {
        pub def lessEqual(x: SingletonSet[a], y: SingletonSet[a]): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (_, Top) => true       
            case (Single(x1), Single(y1)) => x1 == y1
            case (_, _) => false
        }
    }

    instance LowerBound[SingletonSet[a]] {
        pub def minValue(): SingletonSet[a] = Bottom
    }

    instance UpperBound[SingletonSet[a]] {
        pub def maxValue(): SingletonSet[a] = Top
    }

    instance JoinLattice[SingletonSet[a]] with Eq[a] {
        /// \/
        pub def leastUpperBound(x: SingletonSet[a], y: SingletonSet[a]): SingletonSet[a] = match (x, y) { 
            case (Bottom, _) => y
            case (_, Bottom) => x
            case (Single(x1), Single(y1)) => if (x1 == y1) Single(x1) else Top
            case _ => Top
        }
    }

    instance MeetLattice[SingletonSet[a]] with Eq[a] {
        /// /\
        pub def greatestLowerBound(x: SingletonSet[a], y: SingletonSet[a]): SingletonSet[a] = match (x, y) { 
            case (Top, _) => y
            case (_, Top) => x
            case (Single(x1), Single(y1)) => if (x1 == y1) Single(x1) else Bottom
            case _  => Bottom
        }
    }



}
