open RescriptCore

let input = "/input/day2"->Utils.makeInput

type game = {
  id: int,
  red: array<int>,
  green: array<int>,
  blue: array<int>,
}

type game2 = {
  id: int,
  canGame: array<bool>,
}
// Part 1
let _ =
  input
  ->Array.map(game => {
    switch game->String.split(":") {
    | [into, detail] =>
      into
      ->String.split(" ")
      ->Array.get(1)
      ->Option.flatMap(gameId => gameId->Int.fromString)
      ->Option.map(gameId => {
        // Js.log(_gameId)

        detail
        ->String.trim
        ->String.split(";")
        ->Array.map(v => v->String.trim->String.split(",")->Array.map(String.trim))
        ->Array.flat
        ->Array.reduce(
          {id: gameId, canGame: []},
          (prev, current) => {
            switch current->String.split(" ") {
            | [count, color] =>
              switch color {
              | "red" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 12,
                  ]),
                }
              | "green" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 13,
                  ]),
                }
              | "blue" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 14,
                  ]),
                }
              | _ => prev
              }
            | _ => prev
            }
          },
        )
      })
    | _ => None
    }
  })
  ->Array.keepSome
  ->Array.filterMap(game => game.canGame->Array.every(v => v) ? Some(game) : None)
  // ->Js.log
  ->Array.reduce(0, (prev, current) => prev + current.id)
  ->Js.log

// Part 2
let _ =
  input
  ->Array.map(game => {
    switch game->String.split(":") {
    | [into, detail] =>
      into
      ->String.split(" ")
      ->Array.get(1)
      ->Option.flatMap(gameId => gameId->Int.fromString)
      ->Option.map(gameId => {
        // Js.log(_gameId)

        detail
        ->String.trim
        ->String.split(";")
        ->Array.map(v => v->String.trim->String.split(",")->Array.map(String.trim))
        ->Array.flat
        ->Array.reduce(
          {id: gameId, red: [], green: [], blue: []},
          (prev, current) => {
            switch current->String.split(" ") {
            | [count, color] =>
              // Js.log(current)
              switch color {
              | "red" => {
                  ...prev,
                  red: prev.red->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | "green" => {
                  ...prev,
                  green: prev.green->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | "blue" => {
                  ...prev,
                  blue: prev.blue->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | _ => prev
              }
            | _ => prev
            }
          },
        )
      })
    | _ => None
    }
  })
  ->Array.keepSome
  ->Array.map(game =>
    game.red->Math.Int.maxMany * game.green->Math.Int.maxMany * game.blue->Math.Int.maxMany
  )
  ->Array.reduce(0, (prev, current) => prev + current)
  ->Js.log
