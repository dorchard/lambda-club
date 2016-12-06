module UnitsExamples
let foo = 1 + 2

let bar n = n*2

let rec even x = 
    if x = 0 then true
    else odd (x - 1)

and odd x = 
    if x = 1 then true
    else even (x - 1)

let rec sum = function 
    | [] -> 0
    | (x :: xs) -> x + sum xs

[<Measure>] type m 
[<Measure>] type s 

let x = 4.0<m>
let t = 1.5<s>
let v = x / t

// let bad = v + x



[<Measure>] type cm
[<Measure>] type inch

let mkInch : float -> float<inch>
    = fun x -> x * (1.0<inch>)

let mkCm : float -> float<cm>
    = fun x -> x * (1.0<cm>)
    
let inchToCm : float<inch> -> float<cm> 
    = fun x -> x * (2.54<cm/inch>)

let square : float<'u> -> float<'u ^ 2>
   = fun x -> x * x

let mult : float<'u> -> float<'v> -> float<'u * 'v>
   = fun x -> fun y -> x * y

let abs : float<'u> -> float<'u>
    = fun x -> if (x < 0.0<_>) then 0.0<_> - x else x






