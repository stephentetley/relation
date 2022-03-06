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

namespace RelLib/Tuple {

    pub def decons2(f: (u1, u2) -> a & ef, x: (u1, u2)): a & ef = 
        let (x1, x2) = x;
        f(x1, x2)

    pub def decons3(f: (u1, u2, u3) -> a & ef, x: (u1, u2, u3)): a & ef = 
        let (x1, x2, x3) = x;
        f(x1, x2, x3)

    pub def decons4(f: (u1, u2, u3, u4) -> a & ef, x: (u1, u2, u3, u4)): a & ef = 
        let (x1, x2, x3, x4) = x;
        f(x1, x2, x3, x4)

    pub def decons5(f: (u1, u2, u3, u4, u5) -> a & ef, x: (u1, u2, u3, u4, u5)): a & ef = 
        let (x1, x2, x3, x4, x5) = x;
        f(x1, x2, x3, x4, x5)

    pub def case2(x: (u1, u2), f: (u1, u2) -> a & ef): a & ef = 
        let (x1, x2) = x;
        f(x1, x2)

    pub def case3(x: (u1, u2, u3), f: (u1, u2, u3) -> a & ef): a & ef = 
        let (x1, x2, x3) = x;
        f(x1, x2, x3)

    pub def case4(x: (u1, u2, u3, u4), f: (u1, u2, u3, u4) -> a & ef): a & ef = 
        let (x1, x2, x3, x4) = x;
        f(x1, x2, x3, x4)

    pub def case5(x: (u1, u2, u3, u4, u5), f: (u1, u2, u3, u4, u5) -> a & ef): a & ef = 
        let (x1, x2, x3, x4, x5) = x;
        f(x1, x2, x3, x4, x5)
}