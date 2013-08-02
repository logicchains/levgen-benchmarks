
TEXT ·Gen2Rands(SB),7,$0
  MOVQ	addr+0(FP), BP 
	MOVL	0(BP),DX
	BYTE 	$0x8d; BYTE $0x4c; BYTE $0x12; BYTE $0x01// lea  0x1(%rdx,%rdx,1),%ecx
	ADDL	DX, DX
	MOVL	DX, AX
	XORL    $0x88888eee,AX 
	ORL	$0x1,DX
	BYTE    $0x0f; BYTE $0x49; BYTE $0xc1// cmovns %ecx,%eax
 	MOVL	AX, new+8(FP)
	
	MOVL	AX,DX
	BYTE 	$0x8d; BYTE $0x4c; BYTE $0x12; BYTE $0x01// lea  0x1(%rdx,%rdx,1),%ecx
	ADDL	DX, DX
	MOVL	DX, AX
	XORL    $0x88888eee,AX 
	ORL	$0x1,DX
	BYTE    $0x0f; BYTE $0x49; BYTE $0xc1// cmovns %ecx,%eax
 	MOVL	AX, new+12(FP)
	MOVL	AX, 0(BP)
	RET
  

/*This assembly program should be included in a folder in gopath/src called goasm, in the same directory as a file called goasm.go that contains:

package goasm
func Gen2Rands(addr *uint32) (res1,res2 uint32)
*/
