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
mutable __aux2 : int;
mutable __aux3 : int;
mutable __aux4 : int;
};;

let main_reset () = {
____dummy____ = 0;
__aux2 = 5;
__aux3 = 6;
__aux4 = 4;
};;

let main_step mem c x k =
let c = ref c in
let x = ref x in
let k = ref k in
let k_when_true_c = ref 0 in
let o = ref 0 in
let z = ref 0 in
let __aux2 = ref mem.__aux2 in
let __aux3 = ref mem.__aux3 in
let __aux4 = ref mem.__aux4 in
(match !c with
|True -> k_when_true_c := !k;

|_ -> ());

(match !c with
|False -> o := !__aux3;
|True -> o := (!__aux2 + 2);
z := ((!__aux4 * 3) + !k_when_true_c);


|_ -> ());

mem.__aux4 <- !o;

mem.__aux3 <- !x;

(match !c with
|True -> mem.__aux2 <- (!z + 1);

|_ -> ());


(!o)
;;

let _ =
let ____buffer____ = ref (____read_file____ Sys.argv.(1)) in
let mem = main_reset () in
let c = ref False in
let x = ref 0 in
let k = ref 0 in
let rec loop () = try
c := scan_bool (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
x := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
k := int_of_string (List.hd !____buffer____);
____buffer____ := List.tl !____buffer____;
let (o) = main_step mem !c !x !k in
print_int o; print_newline ();
loop ()
with Failure _ -> ()
in loop ()
;;