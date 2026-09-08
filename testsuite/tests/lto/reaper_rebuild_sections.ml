(* TEST
 modules = "reaper_rebuild_sections_dep.ml reaper_rebuild_sections_other.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_sections_dep.cmr reaper_rebuild_sections_other.cmr reaper_rebuild_sections.cmr";
 last_flags = "-o reaper_rebuild_sections.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_rebuild_sections.ltosol";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_sections_other.cmr reaper_rebuild_sections.ltosol -reaper-debug-flags sections";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_rebuild_sections_other.reaped.cmx";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_sections_dep.cmr reaper_rebuild_sections.cmr reaper_rebuild_sections.ltosol -reaper-debug-flags sections";
 ocamlopt.opt;

 file = "reaper_rebuild_sections_dep.reaped.cmx";
 file-exists;

 file = "reaper_rebuild_sections.reaped.cmx";
 file-exists;

 check-ocamlopt.opt-output;
*)

(* The solution is sharded per compilation unit; each rebuild must read only
   the sections for the units it needs. The reference file checks, via the
   debug output, that rebuilding the independent unit does not read this unit's
   or the dependency's sections, and that the batched rebuild of those two does
   not read the independent unit's section. *)

let () = assert (Reaper_rebuild_sections_dep.used 41 = 42)
