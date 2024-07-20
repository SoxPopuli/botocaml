module LwtSyntax = struct
  let ( let* ) = Lwt.bind
  let ( let+ ) = Lwt.map

  let ( let& ) lr fn =
    let* res = lr in
    match res with
    | Ok x -> fn x
    | Error e -> Lwt.return (Error e)
  ;;

  let ( let$ ) (r : ('a, 'b) result) fn =
    match r with
    | Ok x -> fn x
    | Error e -> Lwt.return (Error e)
  ;;

  let return = Lwt.return
end

module OptionSyntax = struct
  let ( let* ) = Option.bind

  let ( let& ) r fn =
    match r with
    | Ok x -> fn x
    | Error _ -> None
  ;;

  let return = Option.some
end

module ResultSyntax = struct
  let ( let* ) = Result.bind

  let ( let& ) (o, e) fn =
    match o with
    | Some x -> fn x
    | None -> Error e
  ;;

  let return = Result.ok
end

let ( ||> ) (a, b) fn = fn a b [@@inline always]
let ( |||> ) (a, b, c) fn = fn a b c [@@inline always]

module List = struct
  include List

  let map_first fn lst = List.map (fun (k, v) -> fn k, v) lst
  let map_second fn lst = List.map (fun (k, v) -> k, fn v) lst
  let sort_by_keys cmp lst = List.sort (fun (k1, _) (k2, _) -> cmp k1 k2) lst
end

module type Foldable = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module Joinable (F : Foldable) = struct
  let join ~sep x =
    F.fold_left (fun acc x -> if String.length acc == 0 then x else acc ^ sep ^ x) "" x
  ;;
end

module String = struct
  include String

  let join_array ~sep x =
    let module M = Joinable (Array) in
    M.join ~sep x
  ;;

  let join_seq ~sep x =
    let module M = Joinable (Seq) in
    M.join ~sep x
  ;;

  let join ~sep lst =
    let module M = Joinable (List) in
    M.join ~sep lst
  ;;

  (** strcmp *)
  let compare a b =
    let aseq = String.to_seq a in
    let bseq = String.to_seq b in
    let rec loop x y =
      match x (), y () with
      | Seq.Nil, Seq.Nil -> 0
      | Seq.Cons (_, _), Seq.Nil -> 1
      | Seq.Nil, Seq.Cons (_, _) -> -1
      | Seq.Cons (xh, xt), Seq.Cons (yh, yt) ->
        let xcode = Char.code xh in
        let ycode = Char.code yh in
        if xcode > ycode then 1 else if xcode < ycode then -1 else loop xt yt
    in
    loop aseq bseq
  ;;

  let compare_rev a b = compare b a
  let sort = List.sort compare
  let sort_array = Array.sort compare
  let get_opt s i = if i >= String.length s then None else Some (String.get s i)

  let contains_string ~(needle : string) ~(haystack : string) =
    let rec is_contained haystack_idx needle_idx =
      if needle_idx >= String.length needle
      then (* got to end of needle; so is contained *)
        true
      else if haystack_idx >= String.length haystack
      then false
      else if get_opt needle needle_idx <> get_opt haystack haystack_idx
      then false
      else is_contained (haystack_idx + 1) (needle_idx + 1)
    in
    let rec loop idx =
      if idx >= String.length haystack
      then false
      else if is_contained idx 0
      then true
      else loop (idx + 1)
    in
    loop 0
  ;;

  let filter ~f str =
    StringLabels.fold_left str ~init:"" ~f:(fun acc ch ->
      if f ch then acc ^ String.make 1 ch else acc)
  ;;

  let from_chars chars =
    ListLabels.fold_left chars ~init:"" ~f:(fun acc c -> acc ^ String.make 1 c)
  ;;
end

module Option = struct
  include Option

  (** apply function if option is none *)
  let map_none ~f = function
    | Some x -> Some x
    | None -> Some (f ())
  ;;

  (** apply function if option is none *)
  let bind_none ~f = function
    | Some x -> Some x
    | None -> f ()
  ;;

  let bind ~f = function
    | Some x -> f x
    | None -> None
  ;;

  let expect ~msg = function
    | Some x -> x
    | None -> failwith msg
  ;;
end

let tee fn x =
  fn x;
  x
;;

module Map = struct
  include Map

  module Make (M : Map.OrderedType) = struct
    include Map.Make (M)

    (** Loop through keys until key is found, or list is exausted *)
    let find_with_fallback map ~keys =
      let rec loop = function
        | [] -> None
        | head :: tail ->
          (match find_opt head map with
           | Some x -> Some x
           | None -> loop tail)
      in
      loop keys
    ;;
  end
end
