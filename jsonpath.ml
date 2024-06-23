
type scalar = 
| Null
| Bool of bool 
| String of string 
| Number of string

type t = 
| Scalar of scalar
| Array of t list
| Object of (string * t) list

type jsonpath = string list * scalar

let rec t_of_jsonaf = function
| `Null -> Scalar Null
| `False -> Scalar(Bool(false))
| `True -> Scalar(Bool(true))
| `String s -> Scalar(String(s))
| `Number n -> Scalar(Number(n))
| `Array(elems) -> Array(List.map t_of_jsonaf elems)
| `Object(pairs) -> 
    let map_value (k, v) = (k, t_of_jsonaf v) in
    Object(List.map map_value pairs)
| _ -> failwith "unsupported JSON element"

let rec jsonpaths_of_t: t -> jsonpath list = function
| Scalar s -> [[], s]
| Array elems -> 
    let mapi_value: int -> t -> jsonpath list = fun i t ->
      let path_element = Format.sprintf "[%d]" i in
      List.map (fun (p,s) -> path_element :: p, s) @@ jsonpaths_of_t t
    in
    List.mapi mapi_value elems
    |> List.flatten
| Object pairs -> 
    let map_value: string * t -> jsonpath list = fun (s, t) ->
      (* TODO(burgerdev): only quote if necessary *)
      let path_element = Format.sprintf "%S" s in
      List.map (fun (p,s) -> path_element :: p, s) @@ jsonpaths_of_t t
    in
    List.map map_value pairs
    |> List.flatten

let string_of_scalar: scalar -> string = function
| Null -> "null"
| Bool b -> Format.sprintf "%b" b
| String s -> Format.sprintf "%S" s
| Number n -> n

let string_of_elems elems = 
  String.concat "." @@ "$" :: elems

let string_of_jsonpath (path_elems, scalar) =
  Format.sprintf "%s = %s" (string_of_elems path_elems) (string_of_scalar scalar)

let () = 
  In_channel.input_all stdin
  |> Jsonaf.of_string
  |> t_of_jsonaf
  |> jsonpaths_of_t
  |> List.iter (fun x -> output_string stdout @@ string_of_jsonpath x; output_string stdout "\n")