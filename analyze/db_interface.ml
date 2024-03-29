open Batteries
open Domain_time_statistics

let today = Unix.time ()

let all_sigs dbh =
  PGSQL(dbh) "SELECT signer, signee, ctime FROM sigs"

let get_key_records dbh keyids =
  PGSQL(dbh) "SELECT keyid, puid, ctime, exptime, alg, keylen FROM keys where keyid in $@keyids"

let get_keys_per_period dbh interval_list keyids =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "SELECT * FROM keys where ctime >= $interval_start AND ctime <= $interval_end AND (exptime IS NULL OR exptime > $interval_end) AND (revoktime IS NULL OR revoktime > $interval_end) AND keyid in $@keyids"
    in
      print_endline (Printf.sprintf "get interval %s keys %d" (format_time interval_start) (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_keys_per_period_cid dbh interval_list cid =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "SELECT keys.keyid, keys.version, keys.puid, keys.ctime, keys.exptime, keys.revoktime, keys.alg, keys.keylen FROM keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = $cid AND ctime >= $interval_start AND ctime <= $interval_end AND (revoktime IS NULL OR revoktime > $interval_end)"
    in
      print_endline (Printf.sprintf "get interval %s keys %d" (format_time interval_start) (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_keys_per_period_all dbh interval_list =
  print_endline "get_keys_per_period";
  let get (interval_start, interval_end) =
    let records = 
      PGSQL(dbh) "SELECT keyid, puid, ctime, exptime, alg, keylen FROM keys where ctime >= $interval_start AND ctime <= $interval_end AND (revoktime IS NULL OR revoktime > $interval_end) AND (exptime IS NULL OR exptime > $interval_end)"
    in
      print_endline (Printf.sprintf "get interval %s keys %d" (format_time interval_start) (List.length records));
      (interval_start, records)
  in
    List.map get interval_list

let get_valid_sigs dbh timestamp =
  PGSQL(dbh) "(SELECT signer, signee FROM sigs INNER JOIN keys on sigs.signer = keys.keyid WHERE (keys.revoktime IS NULL OR keys.revoktime > $timestamp) AND (keys.exptime IS NULL OR keys.exptime > $timestamp) AND (sigs.revoktime IS NULL OR sigs.revoktime > $timestamp) AND (sigs.exptime IS NULL OR sigs.exptime > $timestamp)) intersect (SELECT signer, signee FROM sigs INNER JOIN keys on sigs.signee = keys.keyid WHERE (keys.revoktime IS NULL OR keys.revoktime > $timestamp) AND (keys.exptime IS NULL OR keys.exptime > $timestamp) AND (sigs.revoktime IS NULL OR sigs.revoktime > $timestamp) AND (sigs.exptime IS NULL OR sigs.exptime > $timestamp))"

let get_valid_sigs_upto dbh timestamp =
  PGSQL(dbh) "(SELECT signer, signee FROM sigs INNER JOIN keys on sigs.signer = keys.keyid WHERE (keys.revoktime IS NULL OR keys.revoktime > $timestamp) AND (keys.exptime IS NULL OR keys.exptime > $timestamp) AND (sigs.revoktime IS NULL OR sigs.revoktime > $timestamp) AND (sigs.exptime IS NULL OR sigs.exptime > $timestamp) AND keys.ctime < $timestamp AND sigs.ctime < $timestamp) intersect (SELECT signer, signee FROM sigs INNER JOIN keys on sigs.signee = keys.keyid WHERE (keys.revoktime IS NULL OR keys.revoktime > $timestamp) AND (keys.exptime IS NULL OR keys.exptime > $timestamp) AND (sigs.revoktime IS NULL OR sigs.revoktime > $timestamp) AND (sigs.exptime IS NULL OR sigs.exptime > $timestamp) AND keys.ctime < $timestamp AND sigs.ctime < $timestamp)"

let get_valid_signed_keys dbh timestamp =
  PGSQL(dbh) "SELECT distinct keyid FROM keys INNER JOIN sigs on sigs.signer = keys.keyid OR sigs.signee = keys.keyid WHERE (keys.revoktime IS NULL OR keys.revoktime > $timestamp) AND (keys.exptime IS NULL OR keys.exptime > $timestamp) AND (sigs.revoktime IS NULL OR sigs.revoktime > $timestamp) AND (sigs.exptime IS NULL OR sigs.exptime > $timestamp)"

let get_mscc_keys dbh =
  PGSQL(dbh) "SELECT keys.keyid FROM keys INNER JOIN component_ids ON keys.keyid = component_ids.keyid WHERE component_ids.component_id = 0"

let get_uids_per_key_flat dbh keyids =
  PGSQL(dbh) "SELECT uid FROM uids WHERE keyid IN $@keyids"

let get_uids_per_key dbh keyids =
  let rec loop rem res =
    match rem with
      | keyid :: tl ->
	  let uids = PGSQL(dbh) "SELECT uid FROM uids WHERE keyid = $keyid" in
	    loop tl (uids :: res)
      | [] ->
	  res
  in
    loop keyids []

let divide list =
  let rec loop l ll =
    try
      let first, rest = List.split_at 50 l in
	loop rest (first :: ll)
    with _ -> 
      if List.length l > 0 then l :: ll else ll
  in
    loop list []

let divide_et_impera query arguments =
  let workunits = divide arguments in
  let results = List.mapi
    (fun i work -> 
       query work) 
    workunits 
  in
    List.flatten results
