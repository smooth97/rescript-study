open RescriptCore

let input = "/input/day4_sj"->Utils.makeInput

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

// Console.log(partOneResult)

Js.log(Js.Re.test_(%re("/^#([0-9a-f]){6}$/"), "#aaaa10"))

type height = Inch(int) | Cm(int)

let heightFromString = (unit, value) =>
  switch unit {
  | "cm" =>
    switch value >= 150 && value <= 193 {
    | true => Some(Cm(value))
    | _ => None
    }
  | "in" =>
    switch value >= 59 && value <= 76 {
    | true => Some(Inch(value))
    | _ => None
    }
  | _ => None
  }

type eyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth

let eyeColorFromString = value =>
  switch value {
  | "amb" => Some(Amb)
  | "blu" => Some(Blu)
  | "brn" => Some(Brn)
  | "gry" => Some(Gry)
  | "grn" => Some(Grn)
  | "hzl" => Some(Hzl)
  | "oth" => Some(Oth)
  | _ => None
  }

type checkList =
  Byr(int) | Iyr(int) | Eyr(int) | Hgt(height) | Hcl(string) | Ecl(eyeColor) | Pid(string)

let checkListFromString = (label, value) =>
  switch label {
  | "byr" =>
    let num = value->Int.fromString->Option.getOr(0)
    value->String.length === 4 && num >= 1920 && num <= 2002 ? Some(Byr(num)) : None

  | "iyr" =>
    let num = value->Int.fromString->Option.getOr(0)
    value->String.length === 4 && num >= 2010 && num <= 2020 ? Some(Iyr(num)) : None

  | "eyr" =>
    let num = value->Int.fromString->Option.getOr(0)
    value->String.length === 4 && num >= 2020 && num <= 2030 ? Some(Eyr(num)) : None

  | "hgt" => {
      let unitType = value->String.sliceToEnd(~start=value->String.length - 2)
      let value =
        value
        ->String.slice(~start=0, ~end=value->String.length - 2)
        ->Int.fromString
        ->Option.getOr(0)
      switch heightFromString(unitType, value) {
      | Some(v) => Some(Hgt(v))
      | None => None
      }
    }
  | "hcl" => Js.Re.test_(%re("/^#[0-9A-F]{6}$/i"), value) ? Some(Hcl("true")) : None
  | "ecl" =>
    switch eyeColorFromString(value) {
    | Some(v) => Some(Ecl(v))
    | None => None
    }
  | "pid" =>
    value->String.length === 9 && Js.Re.test_(%re("/^[0-9]{9}/g"), value) ? Some(Pid(value)) : None
  | _ => None
  }

// part2 ----------------------------------------

let partTwoResult =
  parsedList
  ->Array.map(item => {
    let temp =
      item
      ->String.split(" ")
      ->Array.map(checkItem => {
        switch checkItem->String.split(":") {
        | [label, value] =>
          let v = checkListFromString(label, value)
          v
        | _ => None
        }
      })
    temp->Array.keepSome
  })
  ->Array.filter(arr => arr->Array.length == 7)
  ->Array.length
  ->Js.log

// Console.log2("partTwoResult", partTwoResult->Array.length)
