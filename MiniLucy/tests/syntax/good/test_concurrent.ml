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

let n_step mem a b c =
let a = ref a in
let b = ref b in
let c = ref c in
let d = ref 0 in
let e = ref 0 in
let f = ref 0 in
let g = ref 0 in
let out1 = ref 0 in
let out2 = ref 0 in
d := (!a + !b);

f := !b;

e := ((!a + !d) + !c);

g := ((!e + !b) + !f);

out2 := (!e + !f);

out1 := (!g + !c);


(!out1, !out2)
;;

type main_mem = {
____dummy____ : int;
mutable __aux2 : n_mem;
};;

let main_reset () = {
____dummy____ = 0;
__aux2 = n_reset ();
};;

let main_step mem a b c =
let a = ref a in
let b = ref b in
let c = ref c in
let o1 = ref 0 in
let o2 = ref 0 in
let (arg0, arg1) = n_step mem.__aux2 !a !b !c in
o1 := arg0;
o2 := arg1;


(!o1, !o2)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let a = ref 0 in
let b = ref 0 in
let c = ref 0 in
let rec loop () = try
a := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
b := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
c := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (o1, o2) = main_step mem !a !b !c in
print_int o1; print_newline ();
print_int o2; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;