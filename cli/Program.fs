open parser.Parser

let xml = """
<root>
    <child name="child1" />
    <child name="child2" />
</root>
"""

let result = parse xml

match result with
| Some e -> printfn "%A" e
| None -> printfn "Failed to parse"