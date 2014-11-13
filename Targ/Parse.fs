module Targ.Parse

open System.Text.RegularExpressions

type private Args =
    | Flag of char * Args
    | Other of char * Args
    | End

let private letter = Regex(@"[A-Za-z]", RegexOptions.Compiled)
let internal (|Letter|_|) (c : char) =
    if letter.IsMatch(string c) then
        Some c
    else
        None

let private (|Flag|_|) cs =
    match cs with
    | '-'::(Letter a)::t ->
        Some (a, t)
    | _ -> None

let private parseArgs (args : string) =
    let rec inner cs =
        match cs with
        | Flag (flag, t) ->
            Flag(flag, inner t)
        | h::t ->
            Other(h, inner t)
        | [] ->
            End
    args |> List.ofSeq |> inner

let private cleanValue (value : char list) =
    let str =
        value
        |> List.rev
        |> List.map string
        |> String.concat ""
    match str.Trim() with
    | "" -> None
    | s -> Some s

let private buildArgMap args =
    let rec inner (map : Map<char, string option>) cFlag (cValue : char list option) args i =
        match args with
        | Args.Flag (f, a) ->
            let newMap =
                match cFlag, cValue with
                | (Some flag, None) -> Map.add flag None map
                | (Some flag, Some value) -> Map.add flag (cleanValue value) map
                | (None, Some value) -> failwithf "Value with no flag set, invalid parser state"
                | None, None -> map
            inner newMap (Some f) None a (i + 1)
        | Other (c, a) ->
            match cFlag with
            | None ->
                failwithf "Value with no flag found at index %i" i
            | Some _ ->
                let newValue =
                    match cValue with
                    | Some value -> Some (c::value)
                    | None -> Some [c]
                inner map cFlag newValue a (i + 1)
        | End ->
            match cFlag, cValue with
            | (Some flag, None) -> Map.add flag None map
            | (Some flag, Some value) -> Map.add flag (cleanValue value) map
            | (None, Some value) -> failwithf "Value with no flag found at index %i" i
            | None, None -> map
    inner Map.empty None None args 0

let parse = parseArgs >> buildArgMap