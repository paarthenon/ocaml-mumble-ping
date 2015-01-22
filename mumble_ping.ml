(*
	
The glory of Mumble Ping

This program sends a UDP message to a specified mumble server

It is run as

./mping <server-domain> <port-number>

This project doesn't really need to be nearly this fancy. Once I got
a barebones ping program running I wanted to see how I would make this
work if I wanted to handle multiple message types. Those who just
want a ping program, feel free to just get rid of all these type definitions
and take action directly in the bitmatch statement. 

*)

(* Hoity Toity type definitions. Look mom, I'm a real programmer *)
type mumble_sub_version = {
	maj: int;
	min: int;
	patch: int
};;

type mumble_resp_ping = {
	version : mumble_sub_version;
	current_users : int;
	max_users : int
};;

(* Eventually this would contain every message types *)
type message = 
	| PingResponse of mumble_resp_ping 
	| Nonsense ;;


(* Constructing the ping message to be translated *)
let ping_message =
	let ident = 9223372036854775807L in (*Turns into 7FFF FFFF*)
	let typ = 0l in (* Int32 of 0 required for a UDP ping *)
	(BITSTRING {
		typ : 32 : bigendian;
		ident : 64 : bigendian
	})

(* Parse the bitstring responses into our cool little messages so we can work with them *)
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
		PingResponse {
			version = 
				{ maj=v_maj;
				  min=v_min;
				  patch=v_patch};
			current_users = Int32.to_int cur_users;
			max_users = Int32.to_int max_users; }
	| { _ } -> Nonsense


(* Now that the hard lifting is done, do some work *)
let act_on_response message = match message with
	| PingResponse p -> 
		(print_string "Received a response from the server\n");
		(Printf.printf "Version: %u.%u.%u \n" p.version.maj p.version.min p.version.patch);
		(Printf.printf "Users active: (%u/%u)\n" p.current_users p.max_users)
	| Nonsense -> print_string "Received invalid response"

let udp_socket =
	Unix.socket Unix.PF_INET Unix.SOCK_DGRAM (Unix.getprotobyname "udp").Unix.p_proto 

let get_addr url port = 
	let addr = (Unix.gethostbyname url).Unix.h_addr_list.(0) in
	Unix.ADDR_INET (addr, port)

let () =
	(* load command line args *)
	let path = Sys.argv.(1) in
	let port = int_of_string Sys.argv.(2) in

	(* create the socket *)
	let sock = udp_socket in

	(* get the addr info *)
	let addr = get_addr path port in

	(* do work *)
	let str = String.create 64 in
	(
		(* dump the message we plan on sending *)
		(Bitstring.hexdump_bitstring Pervasives.stdout ping_message);

		(* send message *)
		let req = Bitstring.string_of_bitstring ping_message in
		ignore (Unix.sendto sock req 0 (String.length req) [] addr);

		(* read response *)
		let retlen, _ = (Unix.recvfrom sock str 0 64 []) in
		(Printf.printf "Got back message with length %d\n" retlen);
		let resp = Bitstring.bitstring_of_string str in
		(Bitstring.hexdump_bitstring Pervasives.stdout resp);

		(* parse response and then act on it *)
		let message = parse_response resp in
		(act_on_response message)
	)
