let read_file filename =
  let f = open_in filename in
  let n = in_channel_length f in
  let s = really_input_string f n in
  close_in f;
  s

let load_template () =
  read_file "gen/template.html"

let item_title line =
  let title_first_index = String.index line '[' + 1 in
  let title_last_index = String.index line ']' in
  String.sub line title_first_index (title_last_index - title_first_index)

let item_file line =
  let file_first_index = String.index line '(' + 1 in
  let file_last_index = String.index line ')' in
  String.sub line file_first_index (file_last_index - file_first_index)

let replace input output =
  Str.global_replace (Str.regexp_string input) output

type entry = {
  input_filename: string;
  output_filename: string;
  title: string;
  page: string;
}

let fix_filename s =
  match s with
  | "Introduction.html" -> "index.html"
  | s -> s

let generate_nav_links entries current =
  List.mapi (fun i entry ->
    let c =
      if entry.input_filename = current then "class='active'"
      else ""
    in
    Printf.sprintf "<li><a %s href='%s'>%d. %s</a></li>" c entry.output_filename (i + 1) entry.title) entries
  |> String.concat "\n"
  |> Printf.sprintf "<ul>%s</ul>"

let _ =
  let template = load_template () in
  let summary = open_in "src/SUMMARY.md" in
  let line = ref "" in
  let get_line () =
    line := (try input_line summary with End_of_file -> "");
    !line
  in
  let is_item line =
    String.length line > 0 && String.get line 0 = '-'
  in
  ignore @@ get_line ();
  ignore @@ get_line ();
  let _ = try Unix.mkdir "./out" 0o0755 with _ -> () in
  let entries = ref [] in
  while get_line () |> is_item do
    let line = !line in
    let title = item_title line in
    let input_filename = item_file line in
    let output_filename = replace "md" "html"  input_filename |> fix_filename in
    let page = read_file (Filename.concat "src" input_filename) in
    let page = Omd.of_string page |> Omd.to_html in
    let page = replace "<!-- CONTENT -->" page template in
    let entry = {input_filename; output_filename; title; page} in
    entries := entry :: !entries;
  done;
  let entries = List.rev !entries in
  let links = generate_nav_links entries in
  List.iteri (fun i entry ->
    let f = open_out (Filename.concat "out" entry.output_filename) in
    let page = replace "<!-- NAV -->" (links entry.input_filename) entry.page in
    let page =
      if i = List.length entries - 1 then page
      else
        let next_page = List.nth entries (i + 1) in
        replace "<!-- NEXT -->" (Printf.sprintf "<a class='next' href='%s'>Next: %s</a>" next_page.output_filename next_page.title) page
    in
    let page =
      if i = 0 then page
      else
        let prev_page = List.nth entries (i - 1) in
        replace "<!-- PREV -->" (Printf.sprintf "<a class='previous' href='%s'>Previous: %s</a>" prev_page.output_filename prev_page.title) page
    in
    output_string f page;
    close_out f) entries;
  print_endline "HTML files have been saved to './out'"
