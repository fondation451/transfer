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


type after_mem = {
____dummy____ : int;
mutable __aux2 : bool;
};;

let after_reset () = {
____dummy____ = 0;
__aux2 = False;
};;

let after_step mem x =
let x = ref x in
let after = ref False in
let __aux2 = ref mem.__aux2 in
after := (!x ||| !__aux2);

mem.__aux2 <- !after;


(!after)
;;

type main_mem = {
____dummy____ : int;
mutable __aux3 : after_mem;
};;

let main_reset () = {
____dummy____ = 0;
__aux3 = after_reset ();
};;

let main_step mem x =
let x = ref x in
let res = ref False in
let (arg0) = after_step mem.__aux3 !x in
res := arg0;


(!res)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let x = ref False in
let rec loop () = try
x := scan_bool (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (res) = main_step mem !x in
print_bool res; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;