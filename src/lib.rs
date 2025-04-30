/// A heterogeneous stack. Elements of the stack can have different types.
pub trait Stack: Sized {
    /// Apply a function to this stack.
    fn run<F: Function<Self>>(self, function: F) -> F::Out {
        function.apply(self)
    }
}

/// The empty stack.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nil;

impl Stack for Nil {}

/// A stack with a value on top of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cons<T, S: Stack> {
    /// The value on top of the stack.
    pub top: T,
    /// The rest of the stack.
    pub rest: S,
}

impl<T, S: Stack> Stack for Cons<T, S> {}

/// A macro for creating stacks.
#[macro_export]
macro_rules! stack {
    () => {
        $crate::Nil
    };
    ($top:expr) => {
        $crate::Cons {
            top: $top,
            rest: $crate::Nil,
        }
    };
    ($top:expr, $($rest:expr),+) => {
        $crate::Cons {
            top: $top,
            rest: stack!($($rest),+),
        }
    };
}

/// A stack function.
pub trait Function<S: Stack> {
    /// The stack returned by this function.
    type Out: Stack;

    /// Apply this function to a stack.
    fn apply(self, stack: S) -> Self::Out;
}

/// The identity function.
#[derive(Debug, Clone, Copy, Default)]
pub struct Id;

impl<S: Stack> Function<S> for Id {
    type Out = S;

    fn apply(self, stack: S) -> Self::Out {
        stack
    }
}

/// Composition of two functions.
#[derive(Debug, Clone, Copy, Default)]
pub struct Composition<F, G>(pub F, pub G);

impl<F, G, S: Stack> Function<S> for Composition<F, G>
where
    F: Function<S>,
    G: Function<F::Out>,
{
    type Out = G::Out;

    fn apply(self, stack: S) -> Self::Out {
        self.1.apply(self.0.apply(stack))
    }
}

/// Composition of multiple functions.
#[macro_export]
macro_rules! compose {
    () => {
        $crate::Id
    };
    ($f:expr) => {
        $f
    };
    ($f:expr, $($g:expr),+) => {
        $crate::Composition($f, compose!($($g),+))
    };
}

/// The type of composition of multiple functions.
#[macro_export]
macro_rules! compose_type {
    () => {
        $crate::Id
    };
    ($f:ty) => {
        $f
    };
    ($f:ty, $($g:ty),+) => {
        $crate::Composition<$f, compose_type!($($g),+)>
    };
}

/// Run a program on the empty stack.
#[macro_export]
macro_rules! run {
    ($($f:expr),*) => {
        compose!($($f),*).apply($crate::Nil)
    };
}

// Basic stack operations

/// Push a value onto the stack.
#[derive(Debug, Clone, Copy)]
pub struct Push<T>(pub T);

impl<T, S: Stack> Function<S> for Push<T> {
    type Out = Cons<T, S>;

    fn apply(self, stack: S) -> Self::Out {
        Cons {
            top: self.0,
            rest: stack,
        }
    }
}

/// Pop a value off of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Pop;

impl<T, S: Stack> Function<Cons<T, S>> for Pop {
    type Out = S;

    fn apply(self, stack: Cons<T, S>) -> Self::Out {
        stack.rest
    }
}

/// Duplicate the top value of the stack.
/// The type of the top value must implement `Clone`.
#[derive(Debug, Clone, Copy, Default)]
pub struct Dup;

impl<T: Clone, S: Stack> Function<Cons<T, S>> for Dup {
    type Out = Cons<T, Cons<T, S>>;

    fn apply(self, stack: Cons<T, S>) -> Self::Out {
        Cons {
            top: stack.top.clone(),
            rest: Cons {
                top: stack.top,
                rest: stack.rest,
            },
        }
    }
}

/// Swap the top two values of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Swap;

impl<T, U, S: Stack> Function<Cons<T, Cons<U, S>>> for Swap {
    type Out = Cons<U, Cons<T, S>>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top,
            rest: Cons {
                top: stack.top,
                rest: stack.rest.rest,
            },
        }
    }
}

/// Roll the top three values of the stack.
/// The top value goes to the third position, and the other two values are shifted up.
#[derive(Debug, Clone, Copy, Default)]
pub struct RollUp;

impl<T, U, V, S: Stack> Function<Cons<T, Cons<U, Cons<V, S>>>> for RollUp {
    type Out = Cons<U, Cons<V, Cons<T, S>>>;

    fn apply(self, stack: Cons<T, Cons<U, Cons<V, S>>>) -> Self::Out {
        Cons {
            top: stack.rest.top,
            rest: Cons {
                top: stack.rest.rest.top,
                rest: Cons {
                    top: stack.top,
                    rest: stack.rest.rest.rest,
                },
            },
        }
    }
}

/// Roll the top three values of the stack.
/// The top value goes to the first position, and the other two values are shifted down.
#[derive(Debug, Clone, Copy, Default)]
pub struct RollDown;

impl<T, U, V, S: Stack> Function<Cons<T, Cons<U, Cons<V, S>>>> for RollDown {
    type Out = Cons<V, Cons<T, Cons<U, S>>>;

    fn apply(self, stack: Cons<T, Cons<U, Cons<V, S>>>) -> Self::Out {
        Cons {
            top: stack.rest.rest.top,
            rest: Cons {
                top: stack.top,
                rest: Cons {
                    top: stack.rest.top,
                    rest: stack.rest.rest.rest,
                },
            },
        }
    }
}

// Arithmetic operations

/// Add the top two values of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Add;

impl<T, U: std::ops::Add<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Add {
    type Out = Cons<<U as std::ops::Add<T>>::Output, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top + stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Subtract the top two values of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Sub;

impl<T, U: std::ops::Sub<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Sub {
    type Out = Cons<<U as std::ops::Sub<T>>::Output, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top - stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Negate the top value of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Neg;

impl<T: std::ops::Neg, S: Stack> Function<Cons<T, S>> for Neg {
    type Out = Cons<<T as std::ops::Neg>::Output, S>;

    fn apply(self, stack: Cons<T, S>) -> Self::Out {
        Cons {
            top: -stack.top,
            rest: stack.rest,
        }
    }
}

/// Multiply the top two values of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Mul;

impl<T, U: std::ops::Mul<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Mul {
    type Out = Cons<<U as std::ops::Mul<T>>::Output, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top * stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Divide the top two values of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Div;

impl<T, U: std::ops::Div<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Div {
    type Out = Cons<<U as std::ops::Div<T>>::Output, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top / stack.top,
            rest: stack.rest.rest,
        }
    }
}

// Comparison operations

/// Compare the top two values of the stack for equality.
#[derive(Debug, Clone, Copy, Default)]
pub struct Eq;

impl<T, U: PartialEq<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Eq {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top == stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Compare the top two values of the stack for inequality.
#[derive(Debug, Clone, Copy, Default)]
pub struct Ne;

impl<T, U: PartialEq<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Ne {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top != stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Compare the top two values of the stack for less than.
#[derive(Debug, Clone, Copy, Default)]
pub struct Lt;

impl<T, U: PartialOrd<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Lt {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top < stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Compare the top two values of the stack for less than or equal to.
#[derive(Debug, Clone, Copy, Default)]
pub struct Le;

impl<T, U: PartialOrd<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Le {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top <= stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Compare the top two values of the stack for greater than.
#[derive(Debug, Clone, Copy, Default)]
pub struct Gt;

impl<T, U: PartialOrd<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Gt {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top > stack.top,
            rest: stack.rest.rest,
        }
    }
}

/// Compare the top two values of the stack for greater than or equal to.
#[derive(Debug, Clone, Copy, Default)]
pub struct Ge;

impl<T, U: PartialOrd<T>, S: Stack> Function<Cons<T, Cons<U, S>>> for Ge {
    type Out = Cons<bool, S>;

    fn apply(self, stack: Cons<T, Cons<U, S>>) -> Self::Out {
        Cons {
            top: stack.rest.top >= stack.top,
            rest: stack.rest.rest,
        }
    }
}

// Functional programming

/// Push the quoted function as a value onto the stack.
#[macro_export]
macro_rules! quote {
    ($($f:expr),*) => {
        $crate::Push(compose!($($f),*))
    };
}

/// Apply the top value of the stack as a function to the rest of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Apply;

impl<F, S: Stack> Function<Cons<F, S>> for Apply
where
    F: Function<S>,
{
    type Out = F::Out;

    fn apply(self, stack: Cons<F, S>) -> Self::Out {
        stack.top.apply(stack.rest)
    }
}

/// Quote the top value of the stack into a function that pushes it onto the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct Quote;

impl<T, S: Stack> Function<Cons<T, S>> for Quote {
    type Out = Cons<Push<T>, S>;

    fn apply(self, stack: Cons<T, S>) -> Self::Out {
        Cons {
            top: Push(stack.top),
            rest: stack.rest,
        }
    }
}

/// Compose the top two values of the stack as functions.
#[derive(Debug, Clone, Copy, Default)]
pub struct Compose;

impl<F, G, S: Stack> Function<Cons<F, Cons<G, S>>> for Compose {
    type Out = Cons<Composition<G, F>, S>;

    fn apply(self, stack: Cons<F, Cons<G, S>>) -> Self::Out {
        Cons {
            top: Composition(stack.rest.top, stack.top),
            rest: stack.rest.rest,
        }
    }
}

/// Apply either of two functions depending on the top value of the stack.
#[derive(Debug, Clone, Copy, Default)]
pub struct If;

impl<F, G, S: Stack> Function<Cons<bool, Cons<F, Cons<G, S>>>> for If
where
    F: Function<S>,
    G: Function<S, Out = F::Out>,
{
    type Out = F::Out;

    fn apply(self, stack: Cons<bool, Cons<F, Cons<G, S>>>) -> Self::Out {
        if stack.top {
            stack.rest.rest.top.apply(stack.rest.rest.rest)
        } else {
            stack.rest.top.apply(stack.rest.rest.rest)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_push() {
        assert_eq!(run![Push(1), Push(2), Push(3)], stack![3, 2, 1]);
    }

    #[test]
    fn test_pop() {
        assert_eq!(run![Push(1), Push(2), Push(3), Pop], stack![2, 1]);
    }

    #[test]
    fn test_dup() {
        assert_eq!(run![Push(1), Dup], stack![1, 1]);
    }

    #[test]
    fn test_swap() {
        assert_eq!(run![Push(1), Push(2), Swap], stack![1, 2]);
    }

    #[test]
    fn test_heterogeneous() {
        assert_eq!(run![Push(1), Push("two"), Push(3.0)], stack![3.0, "two", 1]);
    }

    #[test]
    fn test_id() {
        assert_eq!(run![Push(1), Id], stack![1]);
    }

    #[test]
    fn test_composition() {
        assert_eq!(
            run![Push(1), compose![Push(2), Push(3), Swap, Pop]],
            stack![3, 1]
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(run![Push(1), Push(2), Add], stack![3]);
    }

    #[test]
    fn test_sub() {
        assert_eq!(run![Push(2), Push(1), Sub], stack![1]);
    }

    #[test]
    fn test_neg() {
        assert_eq!(run![Push(1), Neg], stack![-1]);
    }

    #[test]
    fn test_mul() {
        assert_eq!(run![Push(2), Push(3), Mul], stack![6]);
    }

    #[test]
    fn test_div() {
        assert_eq!(run![Push(6), Push(3), Div], stack![2]);
    }

    #[test]
    fn test_eq() {
        assert_eq!(run![Push(1), Push(1), Eq], stack![true]);
    }

    #[test]
    fn test_ne() {
        assert_eq!(run![Push(1), Push(2), Ne], stack![true]);
    }

    #[test]
    fn test_lt() {
        assert_eq!(run![Push(1), Push(2), Lt], stack![true]);
    }

    #[test]
    fn test_le() {
        assert_eq!(run![Push(1), Push(1), Le], stack![true]);
    }

    #[test]
    fn test_gt() {
        assert_eq!(run![Push(2), Push(1), Gt], stack![true]);
    }

    #[test]
    fn test_ge() {
        assert_eq!(run![Push(1), Push(1), Ge], stack![true]);
    }

    #[test]
    fn test_apply() {
        assert_eq!(
            run![Push(1), quote![Push(2), Push(3), Swap, Pop], Apply],
            stack![3, 1]
        );
    }

    #[test]
    fn test_quote() {
        assert_eq!(run![Push(1), Quote, Apply], stack![1]);
    }

    #[test]
    fn test_compose() {
        assert_eq!(
            run![Push(1), quote![Push(2)], quote![Push(3)], Compose, Apply],
            stack![3, 2, 1]
        );
    }

    #[test]
    fn test_if() {
        assert_eq!(
            run![quote![Push(1)], quote![Push(2)], Push(true), If],
            stack![1]
        );
        assert_eq!(
            run![quote![Push(1)], quote![Push(2)], Push(false), If],
            stack![2]
        );
    }

    #[test]
    fn test_church_bool() {
        let true_ = compose![Pop];
        let false_ = compose![Swap, Pop];
        let if_ = Apply;

        assert_eq!(run![Push(1), Push(2), Push(true_), if_], stack![1]);
        assert_eq!(run![Push(1), Push(2), Push(false_), if_], stack![2]);

        let and = compose![Push(false_), Swap, Apply];
        let or = compose![Push(true_), RollUp, Apply];
        let not = compose![Push(false_), Push(true_), RollDown, Apply];

        assert_eq!(
            run![Push(1), Push(2), Push(true_), Push(true_), and, if_],
            stack![1]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(true_), Push(false_), and, if_],
            stack![2]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(false_), Push(true_), and, if_],
            stack![2]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(false_), Push(false_), and, if_],
            stack![2]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(true_), Push(true_), or, if_],
            stack![1]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(true_), Push(false_), or, if_],
            stack![1]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(false_), Push(true_), or, if_],
            stack![1]
        );
        assert_eq!(
            run![Push(1), Push(2), Push(false_), Push(false_), or, if_],
            stack![2]
        );
        assert_eq!(run![Push(1), Push(2), Push(true_), not, if_], stack![2]);
        assert_eq!(run![Push(1), Push(2), Push(false_), not, if_], stack![1]);
    }

    #[test]
    fn test_church_numerals() {
        let zero = compose![Pop, quote![Id]];
        let one = compose![];
        let two = compose![Dup, Compose];
        let three = compose![Dup, Dup, Compose, Compose];

        assert_eq!(run![Push(0), quote![Push(1), Add], zero, Apply], stack![0]);
        assert_eq!(run![Push(0), quote![Push(1), Add], one, Apply], stack![1]);
        assert_eq!(run![Push(0), quote![Push(1), Add], two, Apply], stack![2]);
        assert_eq!(run![Push(0), quote![Push(1), Add], three, Apply], stack![3]);

        let succ = compose![quote![Dup], Swap, Compose, quote![Compose], Compose];

        assert_eq!(
            run![
                Push(0),
                quote![Push(1), Add],
                Push(zero),
                succ,
                Apply,
                Apply
            ],
            stack![1]
        );
        assert_eq!(
            run![Push(0), quote![Push(1), Add], Push(one), succ, Apply, Apply],
            stack![2]
        );
        assert_eq!(
            run![Push(0), quote![Push(1), Add], Push(two), succ, Apply, Apply],
            stack![3]
        );

        let plus = compose![quote![succ], Swap, Apply, Apply];
        let times = Compose;
        let power = Apply;

        assert_eq!(
            run![
                Push(0),
                quote![Push(1), Add],
                Push(two),
                Push(three),
                plus,
                Apply,
                Apply
            ],
            stack![5]
        );
        assert_eq!(
            run![
                Push(0),
                quote![Push(1), Add],
                Push(two),
                Push(three),
                times,
                Apply,
                Apply
            ],
            stack![6]
        );
        assert_eq!(
            run![
                Push(0),
                quote![Push(1), Add],
                Push(two),
                Push(three),
                power,
                Apply,
                Apply
            ],
            stack![8]
        );
    }

    // // This would work in an untyped language, but unfortunately Rust's type system
    // // doesn't allow us to do this.
    // #[test]
    // fn test_factorial() {
    //     let factorial = compose![
    //         quote![
    //             Swap,
    //             Dup,
    //             Push(0),
    //             Eq,
    //             quote![Pop, Pop, Push(1)],
    //             quote![Dup, Push(1), Sub, RollDown, Dup, Apply, Mul],
    //             RollDown,
    //             If
    //         ],
    //         Dup,
    //         Apply
    //     ];

    //     assert_eq!(run![Push(0), factorial], stack![1]);
    //     assert_eq!(run![Push(1), factorial], stack![1]);
    //     assert_eq!(run![Push(2), factorial], stack![2]);
    //     assert_eq!(run![Push(3), factorial], stack![6]);
    //     assert_eq!(run![Push(4), factorial], stack![24]);
    //     assert_eq!(run![Push(5), factorial], stack![120]);
    // }
}
