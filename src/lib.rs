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
#[derive(Debug, Clone, Copy)]
pub struct Id;

impl<S: Stack> Function<S> for Id {
    type Out = S;

    fn apply(self, stack: S) -> Self::Out {
        stack
    }
}

/// Composition of two functions.
#[derive(Debug, Clone, Copy)]
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

/// Run a program on the empty stack.
#[macro_export]
macro_rules! run {
    ($($f:expr),+) => {
        compose!($($f),+).apply($crate::Nil)
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
#[derive(Debug, Clone, Copy)]
pub struct Pop;

impl<T, S: Stack> Function<Cons<T, S>> for Pop {
    type Out = S;

    fn apply(self, stack: Cons<T, S>) -> Self::Out {
        stack.rest
    }
}

/// Duplicate the top value of the stack.
/// The type of the top value must implement `Clone`.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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

// Arithmetic operations

/// Add the top two values of the stack.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
    ($($f:expr),+) => {
        $crate::Push(compose!($($f),+))
    };
}

/// Apply the top value of the stack as a function to the rest of the stack.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
pub struct Compose;

impl<F, G, S: Stack> Function<Cons<F, Cons<G, S>>> for Compose
where
    G: Function<S>,
    F: Function<G::Out>,
{
    type Out = Cons<Composition<G, F>, S>;

    fn apply(self, stack: Cons<F, Cons<G, S>>) -> Self::Out {
        Cons {
            top: Composition(stack.rest.top, stack.top),
            rest: stack.rest.rest,
        }
    }
}

/// Apply either of two functions depending on the top value of the stack.
#[derive(Debug, Clone, Copy)]
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
            run![Push(1), quote![Push(2)], Dup, Compose, Apply],
            stack![2, 2, 1]
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
        let true_ = quote![Pop, Apply];
        let false_ = quote![Swap, Pop, Apply];
        let if_ = Apply;

        assert_eq!(
            run![quote![Push(1)], quote![Push(2)], true_, if_],
            stack![1]
        );
        assert_eq!(
            run![quote![Push(1)], quote![Push(2)], false_, if_],
            stack![2]
        );
    }
}
