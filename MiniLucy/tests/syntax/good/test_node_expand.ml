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


type n_mem = {
____dummy____ : int;
};;

let n_reset () = {
____dummy____ = 0;
};;

let n_step mem x y =
let x = ref x in
let y = ref y in
let a = ref 0 in
let b = ref 0 in
let v1 = ref 0 in
let v2 = ref 0 in
v1 := (!x + !y);

v2 := (!x * !y);

a := (!v1 * !x);

b := (!v2 * !y);


(!a, !b)
;;

type main_mem = {
____dummy____ : int;
mutable __aux2 : n_mem;
};;

let main_reset () = {
____dummy____ = 0;
__aux2 = n_reset ();
};;

let main_step mem i1 i2 =
let i1 = ref i1 in
let i2 = ref i2 in
let o1 = ref 0 in
let o2 = ref 0 in
let (arg0, arg1) = n_step mem.__aux2 !i1 !i2 in
o1 := arg0;
o2 := arg1;


(!o1, !o2)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let i1 = ref 0 in
let i2 = ref 0 in
let rec loop () = try
i1 := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
i2 := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (o1, o2) = main_step mem !i1 !i2 in
print_int o1; print_newline ();
print_int o2; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;