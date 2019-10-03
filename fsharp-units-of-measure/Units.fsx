// LambdaClub 07/12/2016 - University of Kent, School of Computing
// Dominic Orchard

module Units

// To run in interactive mode with Visual Studio Code do 
// Ctrl+P (or Meta+P if you are using a Mac keyboard)
// then type >fsi start
// Then highlight an area and hit Alt+Enter to evaluate in the REPL
// or just Alt+Enter at the end of a single-liner

// Some simple expressions
let foo = 1 + 2
let dub n = n*2
let mult n m = n * m

// Example of a mutually recursive function
let rec even x = 
  if x = 0 then true
  else odd (x - 1)

and odd x = 
    if x = 1 then true
    else if x = 0 then false
    else even (x - 1)

even 42
odd 0

// Working with lists and polymorphism
let rec sum = function 
   [] -> 0
 | (x :: xs) -> x + sum xs

sum [1; 2; 3]

let rec length = function 
     [] -> 0
   | x :: xs -> 1 + length xs

// Simple units-of-measure type example with metres
// and seconds

[<Measure>] type m 
[<Measure>] type s

let x = 4.0<m>
let t = 1.5<s>
let v = x / t
// let bad = v + x

// Unit-polymorphic squaring
let square (x : float<'u>) = x * x // type annotation not needed : float<'u ^ 2>

let y = 4.0<m>
let w = 3.0
let v0' = y + w

let a = 5.0<m>
let b = a * 4.5


let rec scalarMult (x : float<'u>, n : int) =
    if n < 0 then 0.0<_>
    else x + scalarMult (x, n - 1)


let y' = 4.0<1>
let v0'' = y' + w

let square' (x : float, y : float<'u>) = x * y


let z = x / x : float<1> // Dimensionless unit

// The following demonstrates that float and float<1> are 
// considered equal types
let floatId (x : float<1>) = x 
floatId 1.0

// Cast to a unitful value
let mkMetre : float -> float<m>
    = fun x -> x * (1.0<m>)

// On conversions
[<Measure>] type cm
let cmToM : float<cm> -> float<m>
   = fun x -> x / 100.0<cm/m>

[<Measure>] type inch
let inToCmRatio = 2.54<cm/inch>
let inToCm : float<inch> -> float<cm>
   = fun x -> x * inToCmRatio

// Generically creating conversions based on a ratio
let conversion : float<'u/'v> -> 
                 ((float<'u> -> float<'v>) * (float<'v> -> float<'u>))
    = fun ratio -> ((fun x -> x / ratio) , (fun x -> x * ratio))


// The type system includes expected equational theory on
// units. The following demonstrates commutativity
let multU : float<'u> -> float<'v> -> float<'u 'v>
   = fun x -> fun y -> y * x

// An interesting polymorphic case: how to define 'abs'
// This shows that zero is the only constant which can
// be given a generic type
let abs : float<'u> -> float<'u> 
  = fun x -> if (x < 0.0<_>) then 0.0<_> - x else x

// Not allowed
//let weird : float<'u> -> float<'u>
//  = fun x -> x + 1.0<_>
