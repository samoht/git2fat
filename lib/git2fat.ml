(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open GitTypes
module FS = Fat.Fs.Make(Block)(Io_page)
open FS

let (>>=?) m f = m >>= function
  | `Error e -> fail (Failure "???")
  | `Ok x    -> f x

let (>>=*) m f = m >>= function
  | `Error e -> fail (Failure (Fat.S.Error.to_string e))
  | `Ok x    -> f x

let string_of_error = function
  | `Block_device _ -> "block_device"
  | `Directory_not_empty _ -> "directory-not-empty"
  | `File_already_exists _ -> "file-already-exists"
  | `Format_not_recognised _ -> "format-not-recognized"
  | `Is_a_directory _ -> "is-a-directory"
  | `No_directory_entry _ -> "no-directory-entry"
  | `No_space -> "no-space"
  | `Not_a_directory _ -> "not-a-directory"
  | `Unknown_error _ -> "unknown"

let write_file fs path _perm blob =
  let path = List.tl path in
  let file = String.concat "/" path in
  Printf.printf "write %s\n%!" file;
  let data = Cstruct.of_string (Blob.to_string blob) in
  Lwt_list.fold_left_s (fun path d ->
      match path with
      | []   -> return [ d ]
      | path ->
        let dir = String.concat "/" (List.rev path) in
        begin FS.mkdir fs dir >>= function
          | _ -> return_unit
            (* XXX
              | `File_already_exists _)
              | `Ok ()   -> return_unit
              | `Error e -> fail (Fs_error e) *)
        end >>= fun () ->
        return (d :: path)
    ) [] path
  >>= fun _ ->
  FS.create fs file >>=* fun () ->
  FS.write fs file 0 data >>=* fun () ->
  return_unit

 (* XXXX *)
let size =
  Int64.(mul 16L (mul 1024L 1024L))

let alloc bytes =
  let pages = Io_page.(to_cstruct (get ((bytes + 4095) / 4096))) in
  Cstruct.sub pages 0 bytes

let empty_fs filename =
  Lwt_unix.openfile filename [ Unix.O_CREAT; Unix.O_RDWR ] 0o0644 >>= fun fd ->
  Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
  let message = "All work and no play makes Dave a dull boy.\n" in
  let sector = alloc 512 in
  for i = 0 to 511 do
    Cstruct.set_char sector i (message.[i mod (String.length message)])
  done;
  Block.really_write fd sector >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Block.connect filename >>=? fun dev ->
  FS.connect dev >>=* fun fs ->
  Printf.printf "Created %s\n%!" filename;
  FS.format fs size >>=* fun () ->
  Printf.printf "Filesystem of size %Ld created\n%!" size;
  return fs

type _ t =
  | Local of GitLocal.t
  | Memory of GitMemory.t

let write_fs_aux (type t) (module G: S with type t = t) (t: t) filename commit =
  if Sys.file_exists filename then
    Unix.unlink filename;
  empty_fs filename >>= fun fs ->
  G.references t >>= fun refs ->
  let r = Reference.of_string commit in
  begin
    if commit = "HEAD" then (
      G.read_reference_exn t (Reference.of_string "HEAD")
    ) else if List.mem r refs then
      G.read_reference_exn t r >>= fun sha1 ->
      return sha1
    else
      return (SHA1.of_hex commit)
  end >>= fun init ->
  let init = SHA1.to_commit init in
  G.iter_blobs t ~f:(write_file fs) ~init

let write_fs = function
  | Local l  -> write_fs_aux (module GitLocal) l
  | Memory m -> write_fs_aux (module GitMemory) m

let clone repository =
  let module Remote = GitUnix.Remote(GitMemory) in
  Remote.clone repository

let create repository =
  if Sys.file_exists repository then
    let repository =
      let d = Sys.getcwd () in
      Sys.chdir repository;
      let e = Sys.getcwd () in
      Sys.chdir d;
      e in
    GitLocal.create ~root:repository () >>= fun t ->
    return (Local t)
  else
    GitMemory.create () >>= fun t ->
    clone t repository >>= fun _ ->
    return (Memory t)

open Cmdliner

let help_sections = [
  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ocaml-git/issues.";
]

let default =
  let doc = "Convert a Git snapshot to a FAT image" in
  let man = [
    `S "DESCRIPTION";
    `P "Use $(b,git2fat --help) for more information on a specific command.";
  ] @  help_sections
  in
  Term.info "git2fat"
    ~version:"1.0.0"
    ~doc
    ~man

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let main =
  let repository =
    let doc = Arg.info
        ~docv:"REPOSITORY"
        ~doc:"Location of the remote repository. Use '.' to denote the local repository."
        [] in
    Arg.(required & pos 0 (some string) None & doc) in
  let commit =
    let doc = Arg.info
        ~docv:"COMMIT"
        ~doc:"Use the given snasphot SHA1 or tag."
        [] in
    Arg.(value & pos 1 string "HEAD" & doc) in
  let filename =
    let doc = Arg.info
        ~docv:"FILENAME"
        ~doc:"The name of the FAT image."
        ["o";"output"] in
    Arg.(value & opt string "fat.img" & doc) in
  let f repository filename commit =
    run begin
      create repository >>= fun t ->
      write_fs t filename commit
    end in
  Term.(pure f $ repository $ filename $ commit)

let () =
  match Term.eval (main, default) with `Error _ -> exit 1 | _ -> exit 0
