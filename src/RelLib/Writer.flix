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

/// NOTE this causes problems for the compiler and is not yet usable for realworld code.

namespace RelLib {
    ///
    /// A Writer monad implemented in CPS that uses `#{}` (empty relation) and `<+>` (relational append)
    /// rather than the Monoid class.
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    pub opaque type Writer[ka: Type, w: SchemaRow, a: Type] = (a -> #{| w} -> ka & Impure) -> ka & Impure


    instance Functor[Writer[ka, w]] {
        pub def map(f: a -> b & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            RelLib/Writer.map(f, ma)
    }


    instance Applicative[Writer[ka, w]] {
        pub def point(x: a): Writer[ka, w, a] =
            RelLib/Writer.point(x)

        pub def ap(mf: Writer[ka, w, a -> b & ef], ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            RelLib/Writer.ap(mf, ma)
    }


    instance Monad[Writer[ka, w]] {
        pub def flatMap(f: a -> Writer[ka, w, b] & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            RelLib/Writer.flatMap(f, ma)
    }




}


namespace RelLib/Writer {

    use RelLib.Writer;
    use RelLib.Writer.{Writer};

    ///
    /// Helper function for Writer's implementations of `map` and `ap`.
    ///
    def apply1(ma: Writer[ka, w, a], k: a -> #{| w} -> ka & Impure): ka & Impure =
        let Writer(f) = ma;
        f(k)


    ///
    /// Returns the result of `ma`.
    ///
    pub def runWriter(ma: Writer[(a, #{| w}), w, a]): (a, #{| w}) & Impure =
        let Writer(f) = ma;
        let cont = (a, w) -> (a, w) as & Impure;
        f(cont)


    ///
    /// Returns just the "output" of `ma`.
    ///
    pub def execWriter(ma: Writer[#{| w}, w, a]): #{| w} & Impure =
        let Writer(f) = ma;
        let cont = (_, w) -> w as & Impure;
        f(cont)

    pub def map(f: a -> b & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
        Writer(k ->
            apply1(ma, (a1, w1) -> k(f(a1) as & Impure, w1))
        ) as & ef


    pub def point(x: a): Writer[ka, w, a]  =
        Writer(k -> k(x, #{}))

    pub def ap(mf: Writer[ka, w, a -> b & ef], ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
        Writer(k ->
            apply1(mf, (f1, w1) ->
                apply1(ma, (a1, w2) ->
                    k(f1(a1) as & Impure, w1 <+> w2)))
        ) as & ef

    pub def flatMap(f: a -> Writer[ka, w, b] & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
        Writer(k ->
            apply1(ma, (a1, w1) ->
                apply1(f(a1) as & Impure, (b1, w2) ->
                    k(b1, w1 <+> w2)))
        ) as & ef


    pub def tell(a: #{| w}): Writer[ka, w, Unit] =
        Writer(k ->
            k(() as & Impure, a)
        )


    pub def whenTrue(test: Bool, ma: Writer[ka, w, Unit]): Writer[ka, w, Unit] = match test {
        case true => ma
        case _    => point()
    }

}
