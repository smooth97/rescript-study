open RescriptCore

let makeResult = lines =>
  lines
  ->Array.map(line => {
    line->String.split("")
  })
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

let input = "/input/day1"->Utils.makeInput

// part 1
input->makeResult

// part 2
let alphabetNumbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
let alphabetToNumber = word =>
  switch word {
  | "one" => Some(1)
  | "two" => Some(2)
  | "three" => Some(3)
  | "four" => Some(4)
  | "five" => Some(5)
  | "six" => Some(6)
  | "seven" => Some(7)
  | "eight" => Some(8)
  | "nine" => Some(9)
  | _ => None
  }

let matchedArray = val =>
  alphabetNumbers->Array.filter(alphabetNumber => alphabetNumber->String.startsWith(val))

type state = Started | Nothing
let _ =
  input
  ->Array.map(line => {
    let (_, _, result) =
      line
      ->String.split("")
      ->Array.reduce(("", Nothing, ""), (prev, current) => {
        let (prevWord, state, newString) = prev
        switch current->Int.fromString {
        | Some(_) => ("", Nothing, newString ++ current)
        | None =>
          switch state {
          | Nothing =>
            switch current->matchedArray->Array.get(0) {
            | Some(_) => (current, Started, newString)
            | None => ("", Nothing, newString)
            }
          | Started => {
              let currentWord = prevWord ++ current
              switch currentWord->matchedArray->Array.get(0) {
              | Some(val) =>
                if val == currentWord {
                  switch currentWord->alphabetToNumber {
                  | Some(v) =>
                    switch current->matchedArray->Array.length > 0 {
                    // 끝 자리가 다시 matched 라면
                    | true => (current, Started, newString ++ v->Int.toString)
                    | false => ("", Nothing, newString ++ v->Int.toString)
                    }
                  | None => ("", Nothing, newString)
                  }
                } else {
                  (currentWord, Started, newString)
                }
              | None =>
                // [_, _, _, _] case 없다
                // Shift...

                switch currentWord->String.split("") {
                | [_, b, c] =>
                  switch (b ++ c)->matchedArray->Array.get(0) {
                  // Matched Again
                  | Some(_) => (b ++ c, Started, newString)
                  | None =>
                    switch c->matchedArray->Array.get(0) {
                    // Matched Again
                    | Some(_) => (c, Started, newString)
                    | None => ("", Nothing, newString)
                    }
                  }
                | [_, b] =>
                  switch b->matchedArray->Array.get(0) {
                  | Some(_) => (b, Started, newString)
                  | None => ("", Nothing, newString)
                  }
                | _ => ("", Nothing, newString)
                }
              }
            }
          }
        }
      })
    result
  })
  ->makeResult
