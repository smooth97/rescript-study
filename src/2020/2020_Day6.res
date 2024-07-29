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

// Set 자체가 string 중복제거를 해주기 때문에 union이나 intersect를 쓸 필요는 없음
// 네이밍 uniq가 아님
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

    // 항등원

    // 데이터의 본질을 생각해보기
    // 알파벳을 가지고 비교하는 것이기 때문에 이렇게 array의 첫번째를 디폴트로 설정해서 교차하는 것을 찾는 것보다는
    // 무엇을 디폴트로 설정할지 생각해보는 것이 중요함 (ex. "" or "abcdefghijklmnopqrstuvwxyz")
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
