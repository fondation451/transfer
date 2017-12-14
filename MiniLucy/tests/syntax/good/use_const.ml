(* EN TETE *)

open Printf;;

type bool =
|False
|True
;;

let print_bool x = match x with
|False -> print_string "False"
|True -> print_string "True"
;;

let scan_bool x = match x with
|"False" -> False
|"True" -> True
|_ -> assert false
;;

let ____read_file____ f =
  let fd = open_in f in
  let rec loop out =
    try
      loop (out ^ (input_line fd) ^ " ")
    with End_of_file ->
      close_in fd;
      String.split_on_char ' ' (String.sub out 0 ((String.length out) - 1))
  in loop ""
;;

let (&&&) a b =
  match a, b with
  |True, True -> True
  |_ -> False
;;

let (|||) a b =
  match a, b with
  |False, False -> False
  |_ -> True
;;

let my_not a =
  match a with
  |True -> False
  |False -> True
;;


type main_mem = {
____dummy____ : int;
};;

let main_reset () = {
____dummy____ = 0;
};;

let main_step mem x =
let x = ref x in
let t = ref 0 in
let y = ref 0 in
let z = ref 0 in
t := (4 - 2);

z := (3 + 4);

y := ((!x + (2 * !z)) + (3 * !t));


(!y)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let x = ref 0 in
let rec loop () = try
x := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (y) = main_step mem !x in
print_int y; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;