// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Parse.fs"
open Targ

// Define your library scripting code here

type Bob () =
    member val Thing = "bob" with get, set
    member val Thing2 = "bob" with get, set

let b = Bob()

<@@
b.Thing <- "Fred"
b.Thing2 <- "Bob"
@@>