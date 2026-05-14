(* Benchmark: build a set of N random ints in [0, 2^30) for N = step..max_n,
   then measure average lookup time on a fixed pool of random queries.
   Results are written to [bench.csv].

   CSV schema: impl,n,build_time_s,avg_search_time_s *)

type timing = { n : int; build : float; search : float }

exception Break

(* universe of keys *)
let max_val = 1 lsl 30 - 1 (* 2^30 *)

(* sweep parameters *)
let step = 100_000
let max_n = 10_000_000

(* number of point queries per measurement; large enough to drown out
   per-call timer noise but small relative to build cost *)
let n_queries = 10_000

(* Pre-generate the universe of keys once so every impl sees the same
   insertion sequence, and a fixed pool of queries (mix of hits & misses). *)
let all_keys =
  let a = Array.make max_n 0 in
  for i = 0 to max_n - 1 do
    a.(i) <- Random.int max_val
  done;
  a

let queries =
  let a = Array.make n_queries 0 in
  for i = 0 to n_queries - 1 do
    (* half hits, half random misses *)
    if i mod 2 = 0 then a.(i) <- all_keys.(Random.int max_n)
    else a.(i) <- Random.int max_val
  done;
  a

module Bs_phf = Bentley_saxe_transform.Bs_transform.Make (Phf)

type impl = Aa_tree | Bs_phf

let impl_name = function Aa_tree -> "aa_tree" | Bs_phf -> "bs_phf"

let time_it f =
  let t0 = Unix.gettimeofday () in
  let x = f () in
  let dt = Unix.gettimeofday () -. t0 in
  (x, dt)

let bench_aa n =
  let set, build =
    time_it (fun () ->
        let s = ref Aa_tree.empty in
        for i = 0 to n - 1 do
          s := Aa_tree.insert all_keys.(i) !s
        done;
        !s)
  in
  let (), total = time_it (fun () ->
      for i = 0 to n_queries - 1 do
        ignore (Aa_tree.search queries.(i) set)
      done)
  in
  { n; build; search = total /. float_of_int n_queries }

let bench_bs n =
  let set, build =
    time_it (fun () ->
        let s = ref Bs_phf.empty in
        for i = 0 to n - 1 do
          s := Bs_phf.insert !s all_keys.(i)
        done;
        !s)
  in
  let (), total = time_it (fun () ->
      for i = 0 to n_queries - 1 do
        ignore (Bs_phf.search set queries.(i))
      done)
  in
  { n; build; search = total /. float_of_int n_queries }

let bench = function Aa_tree -> bench_aa | Bs_phf -> bench_bs

let export_csv ~filename timings =
  let oc = open_out filename in
  output_string oc "impl,n,build_time_s,avg_search_time_s\n";
  Hashtbl.iter
    (fun impl ts ->
      List.iter
        (fun t ->
          Printf.fprintf oc "%s,%d,%f,%g\n" (impl_name impl) t.n t.build
            t.search)
        (List.rev ts))
    timings;
  close_out oc

let () =
  Random.self_init ();
  let impls = [ Aa_tree; Bs_phf ] in
  let timings = Hashtbl.create (List.length impls) in
  let best_build = Hashtbl.create 128 in

  List.iter
    (fun impl ->
      Hashtbl.add timings impl [];
      (try
         for i = 1 to max_n / step do
           let n = i * step in
           let t = bench impl n in
           Hashtbl.replace timings impl (t :: Hashtbl.find timings impl);
           Printf.printf "%-8s n=%d build=%7.3fs search=%8.3f µs\n%!"
             (impl_name impl) n t.build (t.search *. 1e6);
           (* same early-stop heuristic as the example: bail if this impl
              is >=10x slower than the best impl seen for this n *)
           (match Hashtbl.find_opt best_build n with
           | None -> Hashtbl.add best_build n t.build
           | Some b ->
               if t.build < b then Hashtbl.replace best_build n t.build
               else if t.build >= 10. *. b then raise Break)
         done
       with Break ->
         Printf.printf "%-8s: stopping early (>10x slower than best)\n%!"
           (impl_name impl)))
    impls;

  export_csv ~filename:"bench.csv" timings;
  print_endline "wrote bench.csv"
