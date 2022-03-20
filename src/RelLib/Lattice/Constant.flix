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

namespace RelLib/Lattice/Constant {

    /// TODO - not sure we should be using Wide - must check...

    pub enum Constant[a] with Eq, Order, ToString, Boxable { 
        case Bottom
        case Constant(a)
        case Top
    }

    pub def constant(x: a): Constant[a] = Constant(x)
    
    instance Functor[Constant] {
        pub def map(f: a -> b & ef, x: Constant[a]): Constant[b] & ef = match x {
            case Bottom      => Bottom
            case Constant(a) => Constant(f(a))
            case Top         => Top
        }
    }

    instance PartialOrder[Constant[a]] with Eq[a] {
        pub def lessEqual(x: Constant[a], y: Constant[a]): Bool = match (x, y) {
            case (Bottom, _)                  => true
            case (Constant(x1), Constant(x2)) => x1 == x2
            case (_, Top)                     => true
            case _                            => false
        }
    }

    instance LowerBound[Constant[a]] {
        pub def minValue(): Constant[a] = Bottom
    }

    instance UpperBound[Constant[a]] {
        pub def maxValue(): Constant[a] = Top
    }
    
    instance JoinLattice[Constant[a]] with Eq[a] {
        /// \/
        pub def leastUpperBound(x: Constant[a], y: Constant[a]): Constant[a] = match (x, y) {
            case (Bottom, _)                  => y
            case (_, Bottom)                  => x
            case (Constant(x1), Constant(x2)) => if (x1 == x2) Constant(x1) else Top
            case _                            => Top
        }
    }

    instance MeetLattice[Constant[a]] with Eq[a] {
        /// /\
        pub def greatestLowerBound(x: Constant[a], y: Constant[a]): Constant[a] = match (x, y) {
            case (Top, _)                     => y
            case (_, Top)                     => x
            case (Constant(x1), Constant(x2)) => if (x1 == x2) Constant(x1) else Bottom
            case _                            => Bottom
        }
    }


}