# ocaml-mumble-ping
A simple program to ping mumble and display the output of the ping request.

Typical output:


	[paarth@Garth ocaml-mumble-ping]$ ./mping <mumble-domain>
	00000000  00 00 00 00 7f ff ff ff  ff ff ff ff              |............    |
	Got back message with length 24
	00000000  00 01 02 04 7f ff ff ff  ff ff ff ff 00 00 00 0d  |................|
	00000010  00 00 03 e8 00 02 fd a0  00 00 00 00 00 00 00 00  |................|
	00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
	00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
	00000040  
	Received a response from the server
	Version: 1.2.4 
	Users active: (13/1000)
	[paarth@Garth ocaml-mumble-ping]$ 
