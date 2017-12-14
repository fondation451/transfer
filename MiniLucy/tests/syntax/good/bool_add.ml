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

let main_step mem a b =
let a = ref a in
let b = ref b in
let z = ref 0 in
let __aux2 = ref 0 in
let __aux3 = ref 0 in
(match !a with
|True -> __aux2 := 1;
|False -> __aux2 := 0;

|_ -> ());

(match !b with
|True -> __aux3 := 1;
|False -> __aux3 := 0;

|_ -> ());

z := (!__aux2 + !__aux3);


(!z)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let a = ref False in
let b = ref False in
let rec loop () = try
a := scan_bool (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
b := scan_bool (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (z) = main_step mem !a !b in
print_int z; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;