open Types

type t =
  { access_id : string
  ; access_secret : string
  ; region : Region.t option
  ; role_arn : string option
  }
[@@deriving fields ~getters ~names, make, show, eq]

let default = { access_id = ""; access_secret = ""; region = None; role_arn = None }

(** Merge two records together.
    Prefers values in dst over src *)
let merge ~src ~dst =
  let merge_string a b = if String.length b = 0 then a else b in
  let merge_option a b =
    if Option.is_some b then b else if Option.is_some a then a else None
  in
  { access_id = merge_string src.access_id dst.access_id
  ; access_secret = merge_string src.access_secret dst.access_secret
  ; region = merge_option src.region dst.region
  ; role_arn = merge_option src.role_arn dst.role_arn
  }
;;

module Environment = struct
  let rec getenv_with_fallback keys =
    match keys with
    | [] -> None
    | hd :: tl ->
      (match Sys.getenv_opt hd with
       | Some x -> Some x
       | None -> getenv_with_fallback tl)
  ;;

  let load () =
    let open Utils.OptionSyntax in
    let* access_id = Sys.getenv_opt "AWS_ACCESS_KEY_ID" in
    let* access_secret = Sys.getenv_opt "AWS_SECRET_ACCESS_KEY" in
    let region =
      getenv_with_fallback [ "AWS_REGION"; "AWS_DEFAULT_REGION" ]
      |> Option.map Region.from_string
    in
    return { access_id; access_secret; region; role_arn = None }
  ;;
end

module File = struct
  module Parser = struct
    open Angstrom

    let header = char '[' *> take_till (( = ) ']') <* char ']'

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

  let rec from_values map (values : (string * string) list) =
    let try_from_cache ~cache ~map x =
      match StringMap.find_opt x !cache with
      | Some hit -> `Hit hit
      | None -> `Miss (StringMap.find x map)
    in
    let cache = ref StringMap.empty in
    let rec loop r remaining =
      match remaining with
      | hd :: tail ->
        let new_rec =
          match hd with
          | "aws_access_key_id", x -> { r with access_id = x }
          | "aws_secret_access_key", x -> { r with access_secret = x }
          | "region", x -> { r with region = Some (Region.from_string x) }
          | "role_arn", x -> { r with role_arn = Some x }
          | "source_profile", x ->
            (match try_from_cache ~cache ~map x with
             | `Hit hit -> merge ~src:hit ~dst:r
             | `Miss miss ->
               let result = merge ~src:(from_values map miss) ~dst:r in
               cache := StringMap.add x result !cache;
               result)
          | _ -> r
        in
        loop new_rec tail
      | [] -> r
    in
    loop default values
  ;;

  let from_string content =
    let open Utils.ResultSyntax in
    let* result = Angstrom.parse_string ~consume:All Parser.run content in
    let map = StringMap.empty in
    let sections =
      result
      |> ListLabels.fold_left ~init:map ~f:(fun map (x : Parser.Section.t) ->
        map |> StringMap.add x.header x.values)
    in
    let sections =
      sections |> StringMap.map (fun values -> from_values sections values)
    in
    return sections
  ;;

  let from_channel channel = channel |> In_channel.input_all |> from_string
  let from_path path = path |> In_channel.open_text |> In_channel.input_all |> from_string
end
