open RescriptCore

let input = "/input/day1"->Utils.makeInput

let numberInput = input->Array.map(line =>
  switch line->Int.fromString {
  | Some(number) => number
  | None => 0
  }
)

let result = Array.reduce(numberInput, 0, (prev, current) => prev + current)

Js.Console.log(result)
