namespace parser
    module Parser =
        // A parser is a function that takes a string and returns an optional tuple of a value and a string
        // The value is the result of the parse and the string is the remaining input
        type public Parser<'a> = string -> ('a * string) option
        type public Attribute = string * string list
        type public Element =
            // A single element terminates itself, so it cannot have children
            // Example: <br />
            | Single of string * Attribute list
            // A multi element can have children
            // Example: <div> <p> </p> </div>
            | MultiOpen of string * Attribute list * Element list

            | MultiClose of string

        val public parse: string -> Element option