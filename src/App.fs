module App

open Browser.Dom
open Quantum

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

// Register our listener
myButton.onclick <- fun _ ->
    count <- add1 count
    myButton.innerText <- sprintf "You clicked: %i time(s)" count
