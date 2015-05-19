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
		
@start:
		call initialize
		call drawMap
@@loop:
		mov ax, 0h
		mov es, ax
		mov cx, [Speed]
		add cx, [word ptr es:046ch]
@@wait:
		cmp [GameStatus], Game_Over
		je @@endLoop
		hlt
		cmp cx, [word ptr es:046ch]
		jne @@wait
		call makeTurn
		call drawMap
		jmp @@loop
@@endLoop:
		nop
@@end:
		call restorePageAndMode
		mov dx, [oldInt9Off]
		mov ax, [oldInt9Seg]
		mov ds, ax
		mov ax, 2509h
		int 21h
		ret

		
newInt9 proc
		push ax bx cx dx
		cli
		in al, 60h
		cmp al, 01h
		je @@setGameOver
		cmp al, 48h
		je @@ifUp
		cmp al, 4dh
		je @@ifRight
		cmp al, 50h
		je @@ifDown
		cmp al, 4bh
		je @@ifLeft
		jmp @@end
@@setGameOver:
		mov [GameStatus], Game_Over
		jmp @@end
@@ifUp:
		;mov [Direction], 0h
		jmp @@end
@@ifRight:
		;mov [Direction], 1h
		jmp @@end
@@ifDown:
		;mov [Direction], 2h
		jmp @@end
@@ifLeft:
		;mov [Direction], 3h
		jmp @@end
@@end:
		mov al, 20h ;Send EOI (end of interrupt)
		out 20h, al ; to the 8259A PIC.
		pop dx cx bx ax
		iret
endp	

		
Dir_North equ 0h
Dir_Easth equ 1h
Dir_South equ 2h
Dir_West equ 3h
Dir_Dx db 0, 1, 0, -1
Dir_Dy db -1, 0, 1, 0


;------------Enum object type------------
MapObjectType_None equ 0000h
MapObjectType_Food1 equ 0101h
MapObjectType_Food2 equ 0102h
MapObjectType_Food3 equ 0103h
MapObjectType_Wall1 equ 0201h
MapObjectType_Wall2 equ 0202h
MapObjectType_Wall3 equ 0203h
MapObjectType_SnakePartLeft equ 0A00h
MapObjectType_SnakePartUp equ 0A01h
MapObjectType_SnakePartRight equ 0A02h
MapObjectType_SnakePartDown equ 0A03h
;------------Enum object type------------
Expires_Never equ 0
;-------------Map Description----------------
MapObject struc
	_Type dw MapObjectType_None
	_Expires dw Expires_Never
MapObject ends

MapWidth equ 32d
MapHeight equ 20d
TilePxSize equ 10d
ScreenPxWidth equ 320d
ScreenPxHeight equ 200d

MapSize equ MapHeight * MapWidth
Map MapObject MapSize dup(<>)
;-------------Map Description----------------

;------Colors-------

SnakeColor equ 02h

;------Colors-------

Speed dw 2h
HeadCoords dw ?


Game_Running equ 0h
Game_Paused equ 01h
Game_Over equ 02h

GameStatus db 0h

setMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	mov dh, 0
	mov dl, ah
	mov ah, MapWidth
	mul ah
	add ax, dx
	mov dx, type(MapObject)
	mul dx
	mov dx, bx
	mov bx, ax
	mov Map[bx]._Type, dx
	mov Map[bx]._Expires, cx
	pop dx cx bx ax
	ret
endp

getMapObj proc ; ah = x, al = y  ===> bx = type, cx = expires
	push ax dx
	mov dh, 0
	mov dl, ah
	mov ah, MapWidth
	mul ah
	add ax, dx
	mov dx, type(MapObject)
	mul dx
	mov dx, bx
	mov bx, ax
	mov dx, Map[bx]._Type
	mov cx, Map[bx]._Expires
	mov bx, dx
	pop dx ax
	ret
endp

drawMap proc
	push ax bx cx dx
	mov ax, 0A000h
	mov es, ax
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	
	call getMapObj
	call drawMapObj
	
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

drawMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	cmp bh, 0Ah
	jne @@checkIfFood1
	mov dx, 00001111b
	call drawBox
	jmp @@end
@@checkIfFood1:
	cmp bx, MapObjectType_Food1
	jne @@checkIfFood2
	mov dx, 00110000b
	call drawBox
	jmp @@end
@@checkIfFood2:
	cmp bx, MapObjectType_Food2
	jne @@checkIfFood3
	mov dx, 02Bh
	call drawBox
	jmp @@end
@@checkIfFood3:
	cmp bx, MapObjectType_Food3
	jne @@checkIfObstacle1
	mov dx, 01Bh
	call drawBox
	jmp @@end
@@checkIfObstacle1:
	cmp bx, MapObjectType_Wall1
	jne @@checkIfObstacle2
	mov dx, 11110000b
	call drawBox
	jmp @@end
@@checkIfObstacle2:
	cmp bx, MapObjectType_Wall2
	jne @@checkIfObstacle3
	mov dx, 00Bh
	call drawBox
	jmp @@end
@@checkIfObstacle3:
	cmp bx, MapObjectType_Wall3
	jne @@ifNone
	mov dx, 01010000b
	call drawBox
	jmp @@end
@@ifNone:
	mov dx, 0
	call drawBox
	jmp @@end
@@end:	
	pop dx cx bx ax
	ret
endp

makeTurn proc
		
		ret
endp	
		
initialize proc
		push ax
		call savePageAndMode
		call setInt9Handler
		xor ax, ax
		mov ax, 13h
		int 10h
		pop ax
		ret
endp	

setInt9Handler proc
		push ax bx es
		mov ax, 3509h
		int 21h
		mov [oldInt9Seg], es
		mov [oldInt9Off], bx
		mov ax, 2509h
		mov dx, offset newInt9
		int 21h
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

restorePageAndMode proc
	push ax	
	mov ah, 0
	mov al, [SavedVideoMode]
	int 10h
	mov ah,05h
	mov al, [SavedVideoPage]
	int 10h
	pop ax
	ret
endp
		

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