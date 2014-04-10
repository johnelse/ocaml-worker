open Cmdliner

module Make = functor(Io : Worker_io.IO) -> struct
  type context = {
    task_id: string;
    report_progress: progress:float -> unit Io.t;
  }

  type 'a action = {
    run : context:context -> 'a Io.t;
    apply_args : 'a Term.t -> string Term.t;
    cancel : unit -> unit Io.t;
  }

  let run_wrapper fd_number task_id action =
    let channel = Io.channel_of_int fd_number in
    let report_progress ~progress =
      let progress_string = Printf.sprintf "%0.2f\n" progress in
      Io.write_to_channel ~channel ~data:progress_string
    in
    let context = {
      task_id;
      report_progress;
    } in
    Io.run (action.run ~context)

  let exec action =
    let fd_number =
      let doc = "File descriptor number for progress reporting." in
      Arg.(required & pos 0 (some int) None & info [] ~docv:"FD_NUMBER" ~doc)
    in
    let task_id =
      let doc = "A task ID." in
      Arg.(required & pos 1 (some string) None & info [] ~docv:"TASK_ID" ~doc)
    in
    let target =
      let wrapper =
        (fun fd_number task_id -> run_wrapper fd_number task_id action)
        |> Term.pure
      in
      Term.(action.apply_args (wrapper $ fd_number $ task_id))
    in
    let info =
      let doc = "A spawnable child process" in
      let man = [
        `S "BUGS";
        `P "Submit pull requests to https://github.com/johnelse/ocaml-worker"
      ] in
      Term.info "worker" ~version:"0.1.0" ~doc ~man
    in
    match Term.eval (target, info) with
    | `Error _ -> exit 1
    | `Ok output -> Printf.printf "%s" output
    | _ -> exit 0
end
