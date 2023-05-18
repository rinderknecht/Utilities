(* Displaying code snippets in error messages *)

module Region = SourceLoc.Region

(* Strings containing OCaml code need to be escaped before printed out
   to the terminal, but OCaml escaping function for strings escapes
   the double quotes, so we need to unescape those. *)

let escape s =
  let escaped = String.escaped s in
  let regexp = Str.regexp "\\\\\"" in
  Str.global_replace regexp "\"" escaped

let fprintf = Format.fprintf

let print_code ~(no_colour: bool) ppf (region: Region.t)
               (in_chan: In_channel.t) =
  (* Colours are disabled if the corresponding environment variable or
     CLI flag is set. *)
  let no_color_cli_flag = no_colour in
  let no_color_env = match Sys.getenv_opt "NO_COLOR" with
                       Some value -> value <> ""
                     | None -> false in
  let is_dumb    = match Sys.getenv_opt "TERM" with
                     Some value -> String.equal value "dumb"
                   | None -> false in
  let dont_print_colors = is_dumb || no_color_env || no_color_cli_flag
  and start      = region#start#line
  and start_offs = region#start#offset `Point
  and stop       = region#stop#line
  and stop_offs  = region#stop#offset `Point in
  let print_underline ppf blanks_len line_len =
    let blanks = String.make blanks_len ' ' in
    let line = String.make line_len '^' in
    fprintf ppf "      %s%s\n%!" blanks line
  in
  let rec loop_over_lines current start stop =
    try
      let current = current + 1
      and line    = Stdlib.input_line in_chan (* Trailing "\n" removed *) in
      let width   = String.length line in
      let () =
        if start - 1 <= current && current < stop + 2 then
          let () = fprintf ppf "%3i" current in
          if start <= current && current <= stop then
            if start < current && current < stop then
              let line = escape line in
              if dont_print_colors then (
                fprintf ppf " | %s\n%!" line;
                print_underline ppf 0 (String.length line))
              else fprintf ppf " | \027[1m\027[31m%s\027[0m\n%!" line
            else
              if current = start then
                let before = String.sub line 0 start_offs |> escape in
                fprintf ppf " | %s" before;
                if current = stop then
                  let between =
                    if start_offs >= width then "\n" (* input_line removes \n *)
                    else
                      if start_offs = stop_offs then
                        String.sub line start_offs 1
                      else
                        String.sub line start_offs (stop_offs - start_offs) in
                  let between = escape between
                  and after =
                    if start_offs >= width then ""
                    else
                      if start_offs = stop_offs then
                        String.sub line (stop_offs + 1) (width - stop_offs -1)
                      else
                        String.sub line stop_offs (width - stop_offs) in
                  let after = escape after in
                  if dont_print_colors  then (
                    fprintf ppf "%s%!%s\n" between after;
                    print_underline ppf (String.length before) (String.length between))
                  else fprintf ppf "\027[1m\027[31m%s\027[0m%!%s\n" between after
                else
                  let after =
                    String.sub line start_offs (width - start_offs) in
                  let after = escape after in
                  if dont_print_colors then (
                    fprintf ppf "%s%!\n" after;
                    print_underline ppf (String.length before) (String.length after))
                  else fprintf ppf "\027[1m\027[31m%s\027[0m%!\n" after
              else
                if current = stop then
                  let before = String.sub line 0 stop_offs |> escape in
                  let after  = String.sub line stop_offs (width - stop_offs) in
                  let after  = escape after in
                  fprintf ppf " | ";
                  if dont_print_colors then (
                    fprintf ppf "%s%!%s\n" before after;
                    print_underline ppf 0 (String.length before))
                  else fprintf ppf "\027[1m\027[31m%s\027[0m%!%s\n" before after
                else ()
          else fprintf ppf " | %s\n" (escape line)
      in if current < stop + 2 then
           loop_over_lines current start stop
    with Stdlib.Invalid_argument _msg -> () (* TODO: How to report? *)
       | Stdlib.End_of_file -> () (* Normal exit *)
    in loop_over_lines 0 start stop

let print ~no_colour ppf (region: Region.t) : unit =
  if region#file <> "" then
    fprintf ppf "%s:\n" (region#to_string `Point);
  try
    let in_chan = In_channel.open_text region#file in
    let result  = print_code ~no_colour ppf region in_chan in
    In_channel.close in_chan;
    result
  with Sys_error _msg -> () (* TODO: Report to maintainers? *)
