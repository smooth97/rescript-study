open RescriptCore

let input = "/input/day1"->Utils.makeInput

let numberInput = input->Array.map(line =>
  switch line->Int.fromString {
  | Some(number) => number
  | None => 0
  }
)

// ref array test ------------

// let tempList = ref([])
// let tempList: ref<array<int>> = ref([])

//let result1 = Array.reduce(numberInput, 0, (prev, current) => {
//  tempList := tempList->Array.concat([prev + current])
//  prev + current
//})


// object test ------------
let obj = Js.Obj.empty()

let result1 = numberInput->Array.reduce(Map.make(), (prev, current) => {
  // let key = string_of_int(prev + current)
  // let copy = Js.Obj.assign(obj, {[key]: "test"})
  switch prev->Map.get(current) {
    | Some(_) => prev
    | None => {
      
      prev->Map.set(current, current)
      prev
      }
  }

})

result1->Map.values->Iterator.toArray->Array.reduce(0, (prev, current) => prev +current)->Js.log

// let update = (obj, props) => {
//   open Js.Obj
//   ()->empty->assign(obj)->assign(props)
// }


// while test ------------

//Array.forEach(numberInput, _item => {
//  while Array.some(tempList, a => a === 10) {
//    Array.concat(
//      tempList,
//      switch tempList[Array.length(tempList) - 1] {
//      | Some(number) => number
//      | None => 0
//      },
//    )
//  }
//})

// let result = Array.concat(tempList, [1])

// Js.Console.log2("resul1", result1)
