open Batteries
open Printf

let today = Unix.time ()

let extract_first l = Option.get (List.hd l)

let all_keys_stats dbh =
  let overall = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today)" in
  let rsa_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1" in
  let dsa_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17" in
  let otheralg_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg <> 1 and alg <> 17" in
  let rsa_512 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 512" in
  let rsa_768 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 768" in
  let rsa_1024 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 1024" in
  let rsa_2048 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 2048" in
  let rsa_3072 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 3072" in
  let rsa_4096 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 4096" in
  let usual_keylens = [512l; 768l; 1024l; 2048l; 3072l; 4096l] in
  let rsa_unusual = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen not in $@usual_keylens" in
  let dsa_512 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 512" in
  let dsa_768 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 768" in
  let dsa_1024 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 1024" in
  let dsa_2048 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 2048" in
  let dsa_3072 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 3072" in
  let dsa_4096 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 4096" in
  let dsa_unusual = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen not in $@usual_keylens" in
  let keys_with_expire_date = PGSQL(dbh) "select count(*) from keys where revoktime is null and exptime is not null and exptime > $today" in
  let avg_uids = PGSQL(dbh) "select avg(total) from (select keyid, count(*) as total from uids group by keyid) as uids_per_key" in
    printf "total number of keys %Ld\n" (extract_first overall);
    printf "number of rsa keys %Ld\n" (extract_first rsa_keys);
    printf "number of dsa keys %Ld\n" (extract_first dsa_keys);
    printf "number of keys with other pk algorithms %Ld\n" (extract_first otheralg_keys);
    printf "number of 512-bit rsa keys %Ld\n" (extract_first rsa_512);
    printf "number of 768-bit rsa keys %Ld\n" (extract_first rsa_768);
    printf "number of 1024-bit rsa keys %Ld\n" (extract_first rsa_1024);
    printf "number of 2048-bit rsa keys %Ld\n" (extract_first rsa_2048);
    printf "number of 3072-bit rsa keys %Ld\n" (extract_first rsa_3072);
    printf "number of 4096-bit rsa keys %Ld\n" (extract_first rsa_4096);
    printf "number of rsa keys with unusual key lengths %Ld\n" (extract_first rsa_unusual);
    printf "number of 512-bit dsa keys %Ld\n" (extract_first dsa_512);
    printf "number of 768-bit dsa keys %Ld\n" (extract_first dsa_768);
    printf "number of 1024-bit dsa keys %Ld\n" (extract_first dsa_1024);
    printf "number of 2048-bit dsa keys %Ld\n" (extract_first dsa_2048);
    printf "number of 3072-bit dsa keys %Ld\n" (extract_first dsa_3072);
    printf "number of 4096-bit dsa keys %Ld\n" (extract_first dsa_4096);
    printf "number of dsa keys with unusual key lengths %Ld\n" (extract_first dsa_unusual);
    printf "keys with expire dates %Ld\n" (extract_first keys_with_expire_date);
    printf "average number of uids per key %f\n" (extract_first avg_uids)

let mscc_keys_stats dbh =
  let overall = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and revoktime is null and  (exptime is null or exptime > $today)" in
    print_endline "some_key_stats overall";
  let rsa_keys = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 " in
  let dsa_keys = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 " in
  let otheralg_keys = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg <> 1 and alg <> 17 " in
  let rsa_512 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 512 " in
  let rsa_768 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 768 " in
  let rsa_1024 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 1024 " in
  let rsa_2048 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 2048 " in
  let rsa_3072 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 3072 " in
  let rsa_4096 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 4096 " in
  let usual_keylens = [512l; 768l; 1024l; 2048l; 3072l; 4096l] in
  let rsa_unusual = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime < $today) and alg = 17 and keylen not in $@usual_keylens " in
  let dsa_512 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 512 " in
  let dsa_768 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 768 " in
  let dsa_1024 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 1024 " in
  let dsa_2048 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 2048 " in
  let dsa_3072 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 3072 " in
  let dsa_4096 = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 4096 " in
  let dsa_unusual = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and (exptime is null or exptime < $today) and alg = 17 and keylen not in $@usual_keylens " in
  let keys_with_expire_date = PGSQL(dbh) "select count(*) from keys inner join component_ids on keys.keyid = component_ids.keyid where component_id = 0 and  revoktime is null and exptime is not null and exptime < $today " in
  let avg_uids = PGSQL(dbh) "select avg(total) from (select uids.keyid, count(*) as total from uids inner join component_ids on uids.keyid = component_ids.keyid where component_id = 0 group by uids.keyid) as uids_per_key" in
    printf "total number of keys %Ld\n" (extract_first overall);
    printf "number of rsa keys %Ld\n" (extract_first rsa_keys);
    printf "number of dsa keys %Ld\n" (extract_first dsa_keys);
    printf "number of keys with other pk algorithms %Ld\n" (extract_first otheralg_keys);
    printf "number of 512-bit rsa keys %Ld\n" (extract_first rsa_512);
    printf "number of 768-bit rsa keys %Ld\n" (extract_first rsa_768);
    printf "number of 1024-bit rsa keys %Ld\n" (extract_first rsa_1024);
    printf "number of 2048-bit rsa keys %Ld\n" (extract_first rsa_2048);
    printf "number of 3072-bit rsa keys %Ld\n" (extract_first rsa_3072);
    printf "number of 4096-bit rsa keys %Ld\n" (extract_first rsa_4096);
    printf "number of rsa keys with unusual key lengths %Ld\n" (extract_first rsa_unusual);
    printf "number of 512-bit dsa keys %Ld\n" (extract_first dsa_512);
    printf "number of 768-bit dsa keys %Ld\n" (extract_first dsa_768);
    printf "number of 1024-bit dsa keys %Ld\n" (extract_first dsa_1024);
    printf "number of 2048-bit dsa keys %Ld\n" (extract_first dsa_2048);
    printf "number of 3072-bit dsa keys %Ld\n" (extract_first dsa_3072);
    printf "number of 4096-bit dsa keys %Ld\n" (extract_first dsa_4096);
    printf "number of dsa keys with unusual key lengths %Ld\n" (extract_first dsa_unusual);

    printf "keys with expire dates %Ld\n" (extract_first keys_with_expire_date);
    printf "average number of uids per key %f\n" (extract_first avg_uids)

let main () =
  let dbh = PGOCaml.connect ~database:"wot-all" () in
    print_endline "\nall keys:";
    all_keys_stats dbh;
    print_endline "\n mscc keys:";
    mscc_keys_stats dbh

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
