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

// Console.log(partOneResult)

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
          switch label {
          | "byr" => {
              //   byr(생년월일) – 4자리; 적어도 1920그리고 최대 2002.
              let num = value->Int.fromString->Option.getOr(0)
              value->String.length === 4 && num >= 1920 && num <= 2002
            }
          | "iyr" => {
              //   iyr(발행 연도) - 4자리; 적어도 2010그리고 최대 2020.
              let num = value->Int.fromString->Option.getOr(0)
              value->String.length === 4 && num >= 2010 && num <= 2020
            }
          | "eyr" => {
              //   eyr(만료 연도) - 4자리; 적어도 2020그리고 최대 2030.
              let num = value->Int.fromString->Option.getOr(0)
              value->String.length === 4 && num >= 2020 && num <= 2030
            }
          | "hgt" => {
              //   hgt(높이) - 숫자 뒤에 cm 또는 in.
              //   cm 숫자는 최소 150, 최대 이어야 합니다 193.
              //   in 숫자는 최소 59, 최대 이어야 합니다 76.
              let unitType = value->String.sliceToEnd(~start=value->String.length - 2)
              let unitValue =
                value
                ->String.slice(~start=0, ~end=value->String.length - 2)
                ->Int.fromString
                ->Option.getOr(0)

              switch unitType {
              | "cm" => unitValue >= 150 && unitValue <= 193
              | "in" => unitValue >= 59 && unitValue <= 76
              | _ => false
              }
            }
          //   hcl(머리 색깔) - a #뒤에 정확히 6개의 문자가 옵니다. 0- 9또는 a- f.
          | "hcl" => Js.Re.test_(%re("/#([0-9a-f]){6}$/"), value)
          //   ecl(눈 색깔) - 정확히 다음 중 하나: amb blu brn gry grn hzl oth.
          | "ecl" => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]->Array.includes(value)
          | "pid" =>
            // pid(여권 ID) - 앞에 0이 포함된 9자리 숫자입니다.
            value->String.startsWith("0") && value->String.length === 9
          | _ => false
          }
        | _ => false
        }
      })
    temp->Array.every(isPass => isPass)
  })
  ->Array.filter(isPass => isPass)

Console.log2("partTwoResult", partTwoResult->Array.length)