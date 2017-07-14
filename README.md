# Theorem auto proof (TAP)
## Calculus of Constructions
Our TAP program uses Calculus of Constructions for proving theorems. CoC is a combination of lambda calculus that allows using variables and types equal in definision. 
Every piece in CoC is named `Term`. It can be:
* Variable, or `Var`
* Application, or App (`A B` means that algorythm A applicates to data B)
* Lambda-Abstraction, or Lam (`\x:a.M` means `M[x of type a]`)
* For-All, or Fa (`Fa(x:a.M)` = for all x of type a do M)
* Star, Box (About star and box a bit later)
All that can be easily listed like that: `P = V|P P|[x:a]M|(x:a)M|*|[] -- in order Var|App|Lam|Fa|Star|Box`
Lambda-Abstraction and For-All are right-assotiative, application is left-assotiative.
## Syntax
* Var can be parsed as one or more letters with or without numbers:
    `x --Var`
    `x1 --Var too`
    `xyz, xy12 -- Vars too`
* App is parsed as `(a) b`
* Lam is parsed as `[x:a]m`
* Fa is parsed as `(x:a)m`
* Star is parsed as `*`
* Box is parsed as `[] -- Box 1`
                   `[23] -- Box 23`
## How to use
Actually, we do not know yet.
Awkward