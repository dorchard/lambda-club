(* quick hack *)

open Printf
open Scanf

let rec y f x = f (y f) x

let memo f =
  let cache = Hashtbl.create 101 in
  fun f' x -> begin
    try Hashtbl.find cache x
    with Not_found ->
      let y = f f' x in
      Hashtbl.add cache x y; y
  end

let better a b = match (a, b) with
  | None, c | c, None -> c
  | Some a, Some b ->
      let na = List.length a in
      let nb = List.length b in
      let sa = List.sort compare a in
      let sb = List.sort compare b in
      if na < nb then Some a
      else if na > nb then Some b
      else if sa < sb then Some a
      else Some b

let append x y = match (x, y) with
  | _, None -> None
  | xs, Some ys -> Some (xs @ ys)

let solve solve' = function
  | 1, _ -> Some []
  | n, [] -> None
  | n, x :: xs ->
      let rec loop m ys zss =
        let zs = append ys (solve' (m, xs)) in
        let zss = zs :: zss in
        if m mod x == 0 then
          loop (m / x) (x :: ys) zss
        else zss in
      let zs = loop n [] [] in
      List.fold_left better None zs

let solve x = y (memo solve) x

let rec print_rec x ys =
  printf " %d" x;
  (match ys with
  | [] -> printf "\n"
  | y :: ys -> print_rec (x * y) ys)

let print = function
  | None -> printf "-1\n"
  | Some ys ->
      let ys = List.sort compare ys in
      (match ys with
      | [] -> assert false
      | y :: ys -> printf "1"; print_rec y ys)

let () =
  let ri () = scanf " %d " (fun x->x) in
  let n = ri () in
  let k = ri () in
  let rec read n =
    if n == 0 then [] else ri () :: read (n - 1) in
  let xs = read k in
  let ys = solve (n, xs) in
  print ys
