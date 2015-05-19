model tiny
.386
.code
org 100h
locals @@

@entry:
		jmp @start
		
		SavedVideoPage db ?
		SavedVideoMode db ?
		oldInt9Seg dw ?
		oldInt9Off dw ?
		Direction db 1h
		
@start:
		call initialize
		call createSnake
		ret

Dir_North equ 0h
Dir_Easth equ 1h
Dir_South equ 2h
Dir_West equ 3h
Dir_Dx db 0, 1, 0, -1
Dir_Dy db -1, 0, 1, 0

createSnake proc
		ret
endp
		
newInt9 proc
		push ax bx cx dx
		cli
		in al, 60h
		cmp al, 48h
		je @@ifUp
		cmp al, 4dh
		je @@ifRight
		cmp al, 50h
		je @@ifDown
		cmp al, 4bh
		je @@ifLeft
		jmp @@end
@@ifUp:
		mov [Direction], 0h
		jmp @@end
@@ifRight:
		mov [Direction], 1h
		jmp @@end
@@ifDown:
		mov [Direction], 2h
		jmp @@end
@@ifLeft:
		mov [Direction], 3h
		jmp @@end
@@end:
		mov al, 20h ;Send EOI (end of interrupt)
		out 20h, al ; to the 8259A PIC.
		pop dx cx bx ax
		iret
endp		
		
initialize proc
		push ax
		call savePageAndMode
		call setInt9Handler
		mov ax, 13h
		int 10h
		mov ax, 0A000h
		mov es, ax
		pop ax
		ret
endp	

setInt9Handler proc
		push ax bx es
		mov ax, 3509h
		int 21h
		mov [oldInt9Seg], es
		mov [oldInt9Off], bx
		;mov ax, 2509h
		;mov dx, offset newInt9
		;int 21h
		pop es bx ax
		ret
endp	

savePageAndMode proc
	push ax bx	
	mov ah,0fh
	int 10h
	mov [SavedVideoMode], al
	mov [SavedVideoPage], bh
	pop bx ax
	ret
endp	
		
TilePxSize equ 10d
ScreenPxWidth equ 320d
ScreenPxHeight equ 200d
BoxSize equ TilePxSize
YMultiplier equ (TilePxSize * ScreenPxWidth)

drawBox proc ; ah = x, al = y, dx = color, es = 0A000h
		push ax bx cx dx di
		push dx
		mov cx, ax
		mov ah, 0
		mov dx, YMultiplier
		mul dx
		mov di, ax
		mov al, ch
		mov ah, TilePxSize
		mul ah
		add di, ax
		pop ax ; color
		mov dx, di ; starting di for stosb
		mov bx, 0
@@whileBxLessThanBoxSize:
		cmp bx, BoxSize
		jae @@endWhile
		push ax
		cmp bx, 0
		je @@edge
		cmp bx, (BoxSize - 1)
		je @@edge
		jmp @@notEdge
@@edge:
		mov ax, 0
@@notEdge:
		mov cx, BoxSize
		mov di, dx
		cld
		rep stosb
		mov ax, 0
		dec di
		stosb
		mov di, dx
		stosb
		inc bx
		add dx, ScreenPxWidth
		pop ax
		jmp @@whileBxLessThanBoxSize
@@endWhile:
		pop di dx cx bx ax
		ret
endp

playSoundFromBx proc
	push ax bx cx dx

	mov     al, 10110110b    ; the magic number (use this binary number only!)
	out     43h, al          ; send it to the initializing port 43h timer 2.

	mov     ax, bx           ; move our frequency value into ax.

	out     42h, al          ; send lsb to port 42h.
	mov     al, ah           ; move msb into al
	out     42h, al          ; send msb to port 42h.

	in      al, 61h          ; get current value of port 61h.
	or      al, 00000011b    ; or al to this value, forcing first two bits high.
	out     61h, al          ; copy it to port 61h of the ppi chip
							 ; to turn on the speaker.


	pop dx cx bx ax
	ret
endp

end @entry