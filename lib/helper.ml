open Stdio

let read_file filename = In_channel.with_file filename ~f:In_channel.input_all
