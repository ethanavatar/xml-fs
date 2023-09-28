namespace parser

open System

module Parser =
    // A parser is a function that takes a string and returns an optional tuple of a value and a string
    // The value is the result of the parse and the string is the remaining input
    type Parser<'a> = string -> ('a * string) option
    type Attribute = string * string list
    type Element =
        // A single element terminates itself, so it cannot have children
        // Example: <br />
        | Single of string * Attribute list
        // A multi element can have children
        // Example: <div> <p> </p> </div>
        | MultiOpen of string * Attribute list * Element list
    
        | MultiClose of string

    let is_alphanumeric (c: char): bool =
        Char.IsLetterOrDigit c

    let reversed (s: string): string =
        let len = s.Length
        let rec loop i =
            match i with
            | 0 -> ""
            | _ -> String.init 1 (fun x -> string s.[i - 1]) + loop (i - 1) 
        loop len

    let match_literal (literal: string): Parser<string> =
        fun (input: string) ->
            let literal_len = literal.Length
            let input_len = input.Length
            if literal_len > input_len then
                None
            else
                let prefix = input[0..literal_len]
                let rest = input[literal_len..(input_len - literal_len)]
                match prefix = literal with
                | true -> Some (literal, rest)
                | false -> None

    let match_identifier: Parser<string> =
        fun (input: string) ->
            let valid_char c = is_alphanumeric c || c = '-'
            let reversed_input = reversed input
            let rec loop i =
                match i with
                | 0 -> 0
                | _ ->
                    if valid_char reversed_input[i - 1] then
                        loop (i - 1)
                    else i
            let loop_len = loop input.Length
            let len = input.Length - loop_len
            let prefix = input[0..len]
            let rest = input[len..(input.Length - len)]
            match len > 0 with
            | true -> Some (prefix, rest)
            | false -> None

    let pair (parser1: Parser<'a>) (parser2: Parser<'b>): Parser<'a * 'b> =
        fun (input: string) ->
            match parser1 input with
            | Some(s1, s1') ->
                match parser2 s1' with
                    | Some (s2, s2') -> Some ((s1, s2), s2')
                    | None -> None
            | None -> None

    let map (f: 'a -> 'b) (parser: Parser<'a>): Parser<'b>  =
        fun (input: string) ->
            match parser input with
            | Some (s, s') -> Some (f s, s')
            | None -> None

    let left (parser1: Parser<'a>) (parser2: Parser<'b>): Parser<'a> =
        let f (x, _) = x
        let parser = pair parser1 parser2
        map f parser

    let right (parser1: Parser<'a>) (parser2: Parser<'b>): Parser<'b> =
        let f (_, x) = x
        let parser = pair parser1 parser2
        map f parser

    let one_or_more (parser: Parser<'a>): Parser<'a list> =
        fun (input: string) ->
            let rec loop s s' =
                match parser s' with
                | Some (s'', s''') -> loop (s @ [s'']) s'''
                | None -> (s, s')
            let (s, s') = loop [] input
            match s with
            | [] -> None
            | _ -> Some (s, s')

    let zero_or_more (parser: Parser<'a>) =
        fun (input: string) ->
            let rec loop s s' =
                match parser s' with
                    | Some (s'', s''') -> loop (s @ [s'']) s'''
                    | None -> (s, s')
            let (s, s') = loop [] input
            Some (s, s')

    let any_char: Parser<string> =
        fun (input: string) ->
            let end_index = input.Length - 1
            match input with
            | "" -> None
            | _ -> Some (input[0..1], input[1..end_index])

    let predicate (parser: Parser<'a>) (predicate: 'a -> bool): Parser<'a> =
        fun (input: string) ->
            match parser input with
            | Some (s, s') ->
                if predicate s then
                    Some (s, s')
                else
                    None
            | None -> None
    
    let is_whitespace (c: string): bool =
        Char.IsWhiteSpace c[0]
    
    let whitespace_char: Parser<string> =
        fun (input: string) ->
            predicate any_char is_whitespace input

    let one_or_more_spaces: Parser<string list> =
        fun (input: string) ->
            one_or_more whitespace_char input
    let zero_or_more_spaces: Parser<string list> =
        fun (input: string) ->
            zero_or_more whitespace_char input
    let quoted_string: Parser<string list> =
        fun (input: string) ->
            let quote = match_literal "\""
            let anything_but_quote c = c <> "\""
            let parser = right quote (left (zero_or_more (predicate any_char anything_but_quote)) quote)
            match parser input with
            | Some (s, s') -> Some (s, s')
            | None -> None

    let attribute_pair: Parser<Attribute> =
        fun (input: string) ->
            let equals = match_literal "="
            pair match_identifier (right equals quoted_string) input
 
    let attributes: Parser<Attribute list> =
        fun (input: string) ->
             zero_or_more (right one_or_more_spaces attribute_pair) input
    let element_start: Parser<string * Attribute list> =
        fun (input: string) ->
            let open_bracket = match_literal "<"
            right open_bracket (pair match_identifier attributes) input
            
    let single_element: Parser<Element> =
        fun (input: string) ->
            let single_close = match_literal "/>"
            let element = left element_start single_close
            match element input with
            | Some (s, s') ->
                let f (name, attributes) = Single (name, attributes)
                Some (f s, s')
            | None -> None


    let open_element: Parser<Element> =
        fun (input: string) ->
            let close = match_literal ">"
            let parser = left element_start close
            let f (name, attributes) = Single (name, attributes)
            map f parser input


    let either (parser1: Parser<'a>) (parser2: Parser<'a>): Parser<'a> =
        fun (input: string) ->
            match parser1 input with
            | Some (s, s') -> Some (s, s')
            | None -> parser2 input

    let and_then (parser: Parser<'a>) (f: 'a -> Parser<'b>): Parser<'b> =
        fun (input: string) ->
            match parser input with
            | Some (s, s') -> f s s'
            | None -> None

    let whitespace_wrap (parser: Parser<'a>) =
        fun (input: string) ->
            left (right zero_or_more_spaces parser) zero_or_more_spaces input

    let close_element (expected_name: string): Parser<String> =
        fun (input: string) ->
            let open_bracket = match_literal "</"
            let close_bracket = match_literal ">"
            let expected_name = match_literal expected_name
            right open_bracket (left (expected_name) close_bracket) input

    let rec parent_element: Parser<Element> =
        fun (input: string) ->
            let element input=
                let either_element = either single_element parent_element
                whitespace_wrap either_element input

            and_then open_element (
                fun e ->
                    let (name, attributes) =
                        match e with
                        | Single (name, attributes) -> (name, attributes)
                        | _ -> failwith "Expected Single"
            
                    let children = zero_or_more element
                    let parser = left children (close_element name)
                    let f (children) = MultiOpen (name, attributes, children)
                    map f parser
            ) input

    let element: Parser<Element> =
        fun (input: string) ->
            let either_element = either single_element parent_element
            whitespace_wrap either_element input
    let parse (input: string): Element option =
        match element input with
        | Some (s, s') ->
            match s' with
            | "" -> Some s
            | _ -> None
        | None -> None
