open RescriptCore

let input = "/2020/input/day4"->Utils.makeInput

let parsedList =
  input
  ->Array.map(line => line->String.length == 0 ? "SPLIT" : line)
  ->Array.reduce("", (a, b) => a->Js.String2.concat(" " ++ b))
  ->Js.String2.split("SPLIT")
  ->Array.map(i => i->Js.String2.trim)

let partOneResult =
  parsedList
  ->Array.map(item => {
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    ->Array.map(a => item->String.includes(a))
    ->Array.some(a => a === false)
      ? 0
      : 1
  })
  ->Array.reduce(0, (prev, current) => prev + current)

Console.log(partOneResult)
