open NodeJs
open RescriptCore

let input =
  Fs.readFileSyncWith(Global.dirname ++ "/input/day1", {encoding: "utf8"})
  ->Buffer.toString
  ->String.split("\n")

input
->Array.map(line => line->String.split(""))
->Array.map(characters => characters->Array.filterMap(v => v->Int.fromString(~radix=10)))
->Array.map(numbers => {
  let first = numbers->Array.get(0)
  let last = numbers->Array.get(numbers->Array.length - 1)
  switch (first, last) {
  | (Some(first'), Some(last')) => (first'->Int.toString ++ last'->Int.toString)->Int.fromString
  | _ => None
  }
})
->Array.keepSome
->Array.reduce(0, (acc, current) => acc + current)
->Js.log

// part 2
let re = %re("/[abcdjklmkpqzy]/")
let alphabetNumbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
type state = Started | Nothing
let _ =
  input
  ->Array.map(line => {
    line
    ->String.replaceRegExp(re, "")
    ->String.split("")
    ->Array.reduce(("", Nothing, ""), (prev, current) => {
      let (prevWord, state, newString) = prev
      switch current->Int.fromString {
      | Some(_) => ("", Nothing, newString ++ current)
      | None =>
        switch state {
        | Nothing => (current, Started, newString)
        | Started => {
            let currentWord = prevWord ++ current
            let _matched =
              alphabetNumbers->Array.filter(
                alphabetNumber => alphabetNumber->String.startsWith(currentWord),
              )

            // switch matched->Array.get(0) {
            // | Some(val) if val == currentWord => // Do Something
            //   ("", 2, 3)
            // } /
            Js.log(currentWord)
            (currentWord, Nothing, "")
          }
        }
      }
    })
  })
  ->Js.log
