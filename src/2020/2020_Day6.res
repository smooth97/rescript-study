let input = "tzibqaulrw
bratip
trbhia
rtiab

sgqytjiw
gkwqybtims

tufcqanysox
ovufxnaqt
aenbqfkutgjx
taufvnxqi
nuaqxfmsth

nuwp
waxp"->String.split("\n\n")

let splitAnswersByGroup = answers => {
  answers
  ->Array.map(v => v->String.split(""))
  ->Array.map(i => i->Array.filter(v => v !== "\n"))
}

let uniqAnswers = answers => {
  answers
  ->Array.map(v => v->Set.fromArray)
  ->Array.map(i => i->Set.size)
}

module IntCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let intersectAnswerCount = answers => {
  answers->Array.map(i => {
    let setArray = i->Array.map(v => v->Belt.Set.fromArray(~id=module(IntCmp)))

    switch setArray->Array.get(0) {
    | Some(default) =>
      setArray
      ->Array.reduce(default, (v1, v2) => Belt.Set.intersect(v1, v2))
      ->Belt.Set.size
    | None => 0
    }
  })
}

let partOneResult =
  input
  ->splitAnswersByGroup
  ->uniqAnswers
  ->Array.reduce(0, (acc, v) => acc + v)
  ->Console.log2("Part One Result: ")

let partTwoResult =
  input
  ->Array.map(v => v->String.split("\n"))
  ->Array.map(splitAnswersByGroup)
  ->intersectAnswerCount
  ->Array.reduce(0, (acc, v) => acc + v)
  ->Console.log2("Part Two Result: ")
