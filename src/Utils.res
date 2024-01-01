open NodeJs
open RescriptCore

let makeInput = filePath =>
  Fs.readFileSyncWith(Global.dirname ++ filePath, {encoding: "utf8"})
  ->Buffer.toString
  ->String.split("\n")
