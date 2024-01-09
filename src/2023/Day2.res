open RescriptCore

let input = "/2023/input/day2"->Utils.makeInput

// Part1 -----------------------------------
// 12 red cubes, 13 green cubes, and 14 blue

let partOneResults =
  input
  ->Array.map(line => line->String.split(":"))
  ->Array.map(game => {
    let gameId =
      game
      ->Array.get(0)
      ->Option.map(gameId => {
        gameId
        ->String.split(" ")
        ->Array.get(1)
        ->Option.flatMap(item => item->Int.fromString)
        ->Option.getOr(0)
      })
      ->Option.getOr(0)

    let isPossibleGame =
      game
      ->Array.get(1)
      ->Option.map(item => item->String.split(";"))
      ->Option.map(cubeCaseGroups =>
        cubeCaseGroups
        ->Array.map(
          cubeGroup => {
            cubeGroup
            ->String.trim
            ->String.split(", ")
            ->Array.map(
              item =>
                switch item->String.split(" ") {
                | [count, color] =>
                  switch color {
                  | "red" => count->Int.fromString->Option.getOr(0) <= 12
                  | "green" => count->Int.fromString->Option.getOr(0) <= 13
                  | "blue" => count->Int.fromString->Option.getOr(0) <= 14
                  | _ => false
                  }
                | _ => false
                },
            )
            ->Array.every(isPossible => isPossible)
          },
        )
        ->Array.every(isPossible => isPossible)
      )

    switch Some(true) == isPossibleGame {
    | true => gameId
    | _ => 0
    }
  })
  ->Array.reduce(0, (prev, current) => prev + current)

Console.log2("part 1", partOneResults)

// Part2 -----------------------------------

type game = {
  id: int,
  red: int,
  green: int,
  blue: int,
}

let partTwoResults =
  input
  ->Array.map(line => line->String.split(":"))
  ->Array.map(game => {
    let gameId =
      game
      ->Array.get(0)
      ->Option.map(gameId => {
        gameId
        ->String.split(" ")
        ->Array.get(1)
        ->Option.flatMap(item => item->Int.fromString)
        ->Option.getOr(0)
      })
      ->Option.getOr(0)

    let gameCaseGroup =
      game
      ->Array.get(1)
      ->Option.map(item => item->String.split(";"))
      ->Option.map(cubeCaseGroups => {
        cubeCaseGroups
        ->Array.map(
          cubeGroup => {
            cubeGroup
            ->String.trim
            ->String.split(", ")
            ->Array.reduce(
              {id: gameId, red: 0, blue: 0, green: 0},
              (prev, current) =>
                switch current->String.split(" ") {
                | [count, color] =>
                  switch color {
                  | "red" => {
                      ...prev,
                      red: count->Int.fromString->Option.getOr(0),
                    }
                  | "green" => {
                      ...prev,
                      green: count->Int.fromString->Option.getOr(0),
                    }
                  | "blue" => {
                      ...prev,
                      blue: count->Int.fromString->Option.getOr(0),
                    }
                  | _ => prev
                  }
                | _ => {id: gameId, red: 0, blue: 0, green: 0}
                },
            )
          },
        )
        ->Array.reduce(
          {id: gameId, red: 0, blue: 0, green: 0},
          (prev, current) => {
            ...prev,
            red: prev.red < current.red ? current.red : prev.red,
            blue: prev.blue < current.blue ? current.blue : prev.blue,
            green: prev.green < current.green ? current.green : prev.green,
          },
        )
      })

    gameCaseGroup
  })
  ->Array.map(item => {
    let values = item->Option.getOr({id: 0, red: 0, blue: 0, green: 0})
    values.red * values.green * values.blue
  })
  ->Array.reduce(0, (prev, current) => prev + current)

Console.log2("part 2", partTwoResults)
