type t =
  { access_id : string
  ; access_secret : string
  ; region : Region.t option
  ; role_arn : string option
  }
[@@deriving fields ~getters ~names, make]

let default = { access_id = ""; access_secret = ""; region = None; role_arn = None }

(** Merge two records together.
    Prefers values in dst over src *)
let merge ~src ~dst =
  let merge_string a b = 
    if String.length b = 0 then
      a
    else
      b
  in
  let merge_option a b =
    if Option.is_some b then
      b
    else if Option.is_some a then
      a
    else
      None
  in
  { access_id = merge_string src.access_id dst.access_id
  ; access_secret = merge_string src.access_secret dst.access_secret
  ; region = merge_option src.region dst.region
  ; role_arn = merge_option src.role_arn dst.role_arn
  }
;;

module StringMap = Map.Make (String)

module Parser = struct
  open Angstrom

  let header = char '[' *> many_till any_char (char ']') >>| Utils.String.from_chars

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  ;;

  let whitespace = take_while is_whitespace
  let not_whitespace = take_till is_whitespace

  let key_value_pair =
    take_till (fun ch -> is_whitespace ch || ch = '=')
    <* whitespace
    <* skip (( = ) '=')
    <* whitespace
    >>= fun key -> not_whitespace >>= fun value -> return (key, value)
  ;;

  module Section = struct
    type t =
      { header : string
      ; values : (string * string) list
      }
    [@@deriving eq, show]
  end

  let section =
    header
    <* whitespace
    >>= fun header ->
    sep_by whitespace key_value_pair >>= fun values -> return Section.{ header; values }
  ;;

  let run = whitespace *> sep_by whitespace section <* whitespace
end

let from_values map (values : (string * string) list) =
  let rec loop r remaining =
    match remaining with
    | hd :: tail ->
      let new_rec =
        match hd with
        | "access_id", x -> { r with access_id = x }
        | "access_secret", x -> { r with access_secret = x }
        | "region", x -> { r with region = Some (Region.of_string x) }
        | "role_arn", x -> { r with role_arn = Some x }
        | "source_profile", x -> 
            let src = StringMap.find x map in
            merge ~src ~dst:r
        | _ -> r
      in
      loop new_rec tail
    | [] -> r
  in
  loop default values
;;

let from_file path =
  let open Utils.ResultSyntax in
  let content = In_channel.open_text path |> In_channel.input_all in
  let* result = Angstrom.parse_string ~consume:All Parser.run content in
  let map = StringMap.empty in
  let sections =
    result
    |> List.fold_left
         (fun map (x : Parser.Section.t) -> map |> StringMap.add x.header x.values)
         map
  in
  let sections = sections |> StringMap.mapi (fun header values -> ()) in
  return sections
;;
