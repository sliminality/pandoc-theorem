## Simple block-level theorem

Theorem (Hedberg).

:   Any type with decidable equality is a set.

## Complex block-level theorem

Lemma (Pumping Lemming). \label{lem}

:   Let $L$ be a regular language. Then there exists an integer $p \geq 1$ called the "pumping length" which depends only on $L$, such that every string $w \in L$ of length at least $p$ can be divided into three substrings $w = xyz$ such that the following conditions hold:

    - $|y| \geq 1$
    - $|xy| \leq p$
    - $xy^n z \in L$, for all $n \geq 0$.

    That is, the non-empty substring $y$ occurring within the first $p$ characters of $w$ can be "pumped" any number of times, and the resulting string is always in $L$.

## Single inline theorem

Proof. 
: By induction on the structure of the typing judgment.

## Multiple inline theorems

Def (Agda).
:   A dependently-typed programming language often used for interactive theorem proving.
:   A video game that doesn't mean you understand the underlying theory, according to Bob.

## Regular definition lists still work

Groceries
: Bananas
: Lenses
: Barbed wire

Programming language checklist

:     *Strictures:* Does the language have sufficiently many restrictions? It is always easier to relax strictures later on.

:     *Affordances:* Actually, these don't really matter.

