open RescriptCore

let input = Utils.input5

module Direct = {
  type rowCode = F | B
  type columnCode = L | R

  let getRows = v => {
    v
    ->Array.map(code => {
      switch code {
      | "F" => Some(F)
      | "B" => Some(B)
      | _ => None
      }
    })
    ->Array.keepSome
  }

  let getColumns = v => {
    v
    ->Array.map(code => {
      switch code {
      | "L" => Some(L)
      | "R" => Some(R)
      | _ => None
      }
    })
    ->Array.keepSome
  }
}

let allRow = (0, 127)
let allColumn = (0, 7)

let decode = seatCode => {
  let column =
    seatCode
    ->String.split("")
    ->Direct.getColumns
    ->Array.reduce(allColumn, ((min, max), current) => {
      switch current {
      | L => (min, min + (max - min) / 2)
      | R => (max - (max - min) / 2, max)
      }
    })

  let row =
    seatCode
    ->String.split("")
    ->Direct.getRows
    ->Array.reduce(allRow, ((min, max), current) => {
      switch current {
      | F => (min, min + (max - min) / 2)
      | B => (max - (max - min) / 2, max)
      }
    })

  // row * 8 + column
}

let test = input->Array.get(0)->Option.mapOr((), decode)

// let result =
//   input
//   ->Array.map(decode)
//   ->Array.reduce(0, (prev, current) => {
//     if current > prev {
//       current
//     } else {
//       prev
//     }
//   })
//   ->Js.log
