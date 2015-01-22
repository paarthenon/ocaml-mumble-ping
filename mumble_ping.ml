(* mumble ping *)

open Unix;;

let ping_req =
	let ident = 9223372036854775807L in
	let typ = 0l in
	(BITSTRING {
		typ : 32 : bigendian;
		ident : 64 : bigendian
	})

let parse_response resp =
	bitmatch resp with
	| { 
		v_maj : 16 : bigendian;
		v_min : 8 : bigendian;
		v_patch : 8 : bigendian;
		ident : 64 : bigendian;
		cur_users : 32 : bigendian;
		max_users : 32 : bigendian;
		bandwidth : 32 : bigendian 
	} ->
		(print_string "Received a response from the server\n");
		(Printf.printf "Version: %u.%u.%u \n" v_maj v_min v_patch);
		(Printf.printf "Users active: (%u/%u)\n" (Int32.to_int cur_users) (Int32.to_int max_users))
	| { _ } -> print_string "Received invalid response"


let () =
	let sock = socket PF_INET SOCK_DGRAM (getprotobyname "udp").Unix.p_proto in
	let path = Sys.argv.(1) in
	let addr = (gethostbyname path).h_addr_list.(0) in
	let portaddr = ADDR_INET (addr, 64738) in
	let str = String.create 64 in
	(
		(Bitstring.hexdump_bitstring Pervasives.stdout ping_req);
		let req = Bitstring.string_of_bitstring ping_req in
		sendto sock req 0 (String.length req) [] portaddr;
		let retlen, _ = (recvfrom sock str 0 64 []) in
		(Printf.printf "Got back message with length %d\n" retlen);
		let resp = Bitstring.bitstring_of_string str in
		(Bitstring.hexdump_bitstring Pervasives.stdout resp);
		(parse_response resp)
	)
