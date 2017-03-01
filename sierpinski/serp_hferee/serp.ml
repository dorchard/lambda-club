(* n: depth
 * h: height 
 * i: line number *)
let rec serp n h i = match n with
| 0 -> (print_string (String.make (h-i-1) '_'); 
        print_string (String.make (2*i+1) '1'); 
        print_string (String.make (h-i-1) '_'))
| n -> if i < h/2 
       then
         (print_string(String.make (h/2) '_');
          serp (n-1) (h/2) i;
          print_string(String.make (h/2) '_'))
       else
         (serp (n-1) (h/2) (i - h/2);
          print_string "_";
          serp (n-1) (h/2) (i - h/2));;
 
let rec full_serp size n i = 
  if i < size then 
    (serp n size i; print_newline(); full_serp size n (i+1))
  else print_newline();; 

let rec pow2(n) =
  if n=0 then 1 else 2 * pow2(n-1);;

let main() = 
  if Array.length Sys.argv < 2 then print_string "Please intput integer\n"
  else let n = int_of_string (Sys.argv.(1)) in full_serp (pow2 n) n 0
in main()
