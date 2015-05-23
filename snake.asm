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
		call makeSnake
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
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartDown
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartUp
	call setMapObj
	jmp @@end
@@ifDown:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartUp
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartDown
	call setMapObj
	jmp @@end
@@ifLeft:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartRight
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartLeft
	call setMapObj
	jmp @@end
@@ifRight:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartLeft
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartRight
	call setMapObj
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


Speed dw 1h
HeadCoords dw ?
HeadType dw ?


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

;----------Colors----------
SnakeColor dw 15d
Food1Color dw 48d
Food2Color dw 43d
Wall1Color dw 240d
Wall2Color dw 11d
Wall3Color dw 80d
NoneColor  dw 0d
;----------Colors----------

drawMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	cmp bh, 0Ah
	jne @@checkIfFood1
	mov dx, [SnakeColor]
	call drawBox
	jmp @@end
@@checkIfFood1:
	cmp bx, MapObjectType_Food1
	jne @@checkIfFood2
	mov dx, [Food1Color]
	call drawBox
	jmp @@end
@@checkIfFood2:
	cmp bx, MapObjectType_Food2
	jne @@checkIfWall1
	mov dx, [Food2Color]
	call drawBox
	jmp @@end
@@checkIfWall1:
	cmp bx, MapObjectType_Wall1
	jne @@checkIfWall2
	mov dx, [Wall1Color]
	call drawBox
	jmp @@end
@@checkIfWall2:
	cmp bx, MapObjectType_Wall2
	jne @@checkIfWall3
	mov dx, [Wall2Color]
	call drawBox
	jmp @@end
@@checkIfWall3:
	cmp bx, MapObjectType_Wall3
	jne @@ifNone
	mov dx, [Wall3Color]
	call drawBox
	jmp @@end
@@ifNone:
	mov dx, [NoneColor]
	call drawBox
	jmp @@end
@@end:	
	pop dx cx bx ax
	ret
endp

MaxY equ (MapHeight - 1)
MaxX equ (MapWidth - 1)
makeSnake proc
	push ax bx cx dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	mov cx, Expires_Never
	cmp ah, 0
	je @@edge
	cmp ah, MaxX
	je @@edge
	cmp al, 0
	je @@edge
	cmp al, MaxY
	je @@edge
	jmp @@notEdge
@@edge:
	cmp ah, 0h
	je @@isWall2
	cmp ah, MaxX
	je @@isWall3
	mov bx, MapObjectType_Wall1
	jmp @@done
@@isWall2:
	mov bx, MapObjectType_Wall2
	jmp @@done
@@isWall3:
	mov bx, MapObjectType_Wall3
	jmp @@done
@@notEdge:
	mov bx, MapObjectType_None
@@done:
	call setMapObj
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	mov [HeadCoords], 0907h
	mov [HeadType], MapObjectType_SnakePartRight
	mov cx, 04h
@@loop:
	mov ah, cl
	add ah, 5
	mov al, 7
	mov bx, MapObjectType_SnakePartRight
	call setMapObj
	dec cx
	jcxz @@end
	jmp @@loop
@@end:
	mov ax, 0B08h
	mov bx, MapObjectType_Food1
	mov cx, Expires_Never
	call setMapObj
	mov ax, 1008h
	mov bx, MapObjectType_Food2
	mov cx, Expires_Never
	call setMapObj
	
	pop dx cx bx ax
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

MaxSnakeLength dw 040d
makeTurn proc
	pushf
	push ax bx cx dx
	cli
@@collide:
	mov ax, [HeadCoords]
	call getMapObj
	cmp bh, 0Ah
	je @@alive
	mov [GameStatus], Game_Over
	jmp @@end
@@alive:
	cmp cx, [MaxSnakeLength]
	jb @@notWonYet 
	mov [GameStatus], Game_Over
	jmp @@end
@@notWonYet:
	call getNextAx
	call getMapObj
	mov dx, bx
	call onCollideWith
	cmp di, 1
	je @@collide
	cmp [GameStatus], Game_Over
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	call getNextAx
	mov [HeadCoords], ax
	mov HeadType, bx
	inc cx
	call setMapObj
	call decreaseExpirations
@@end:
	pop dx cx bx ax
	popf
	ret
endp

getNextAx proc ; ax = current coords, bx = snake part
	push bx cx dx
	cmp bx, MapObjectType_SnakePartLeft
	je @@left
	cmp bx, MapObjectType_SnakePartRight
	je @@right
	cmp bx, MapObjectType_SnakePartUp
	je @@up
	cmp bx, MapObjectType_SnakePartDown
	je @@down
	jmp @@end
@@left:
	dec ah
	jmp @@continue
@@right:
	inc ah
	jmp @@continue
@@up:
	dec al
	jmp @@continue
@@down:
	inc al
	jmp @@continue
@@continue:
	call getMapObj
	cmp bx, MapObjectType_Wall3
	jne @@end
	mov ah, 01h
@@end:
	pop dx cx bx
	ret
endp

SelfCollisionAllowed db 0
onCollideWith proc ; ax = who, dx = target type; di => isRecheckNeeded
	push ax bx cx dx
	mov di, 0
	cmp dh, 0Ah
	je @@withUrSelf
	cmp dx, MapObjectType_Food1
	je @@withFood1
	cmp dx, MapObjectType_Food2
	je @@withFood2
	cmp dx, MapObjectType_Wall1
	je @@withWall1
	cmp dx, MapObjectType_Wall2
	je @@withWall2
	cmp dx, MapObjectType_Wall3
	je @@withWall3
	jmp @@end
@@withUrSelf:
	cmp [SelfCollisionAllowed], 1
	je @@end
	mov [GameStatus], Game_Over
	jmp @@end
@@withFood1:
	mov dx, 1
	call changeSnakeDuration
	mov bx, MapObjectType_Food1
	mov cx, Expires_Never
	call generateMapObjWhereEmpty
	jmp @@end
@@withFood2:
	;mov bx, Note_D2
	;call playSoundFromBx
	mov dx, -2
	call changeSnakeDuration
	mov bx, MapObjectType_Food2
	mov cx, Expires_Never
	call generateMapObjWhereEmpty
	jmp @@end
@@withWall1:
	mov [GameStatus], Game_Over
	jmp @@end
@@withWall2:
	call reverseSnake
	mov di, 1
	jmp @@end
@@withWall3:
	jmp @@end
@@turn:
	mov ax, [HeadCoords]
	call setMapObj
	mov di, 1
	jmp @@end
@@die:
	mov [GameStatus], Game_Over
	jmp @@end
@@end:
	pop dx cx bx ax
	ret
endp

teleportHead proc
		push ax
		mov ax, [HeadCoords]
		mov ah, 03h
		mov [HeadCoords], ax
		pop ax
		ret
endp

reverseSnake proc
	push ax bx cx dx
	mov ax, [HeadCoords]
	call getMapObj
	mov dx, cx
	inc dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	call getMapObj
	cmp bh, 0Ah
	jne @@notSnake
	call getOppositeSnakeObj
	cmp cx, 1
	jne @@notNewHead
	mov [HeadType], bx
	mov [HeadCoords], ax
@@notNewHead:
	neg cx
	add cx, dx
	call setMapObj
	jmp @@next
@@notSnake:
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

getOppositeSnakeObj proc
	add bl, 2
	and bl, 3
	ret
endp

generateMapObjWhereEmpty proc
	push ax bx cx dx
	push bx cx
@@generateRandomEmptyCoords:
	call generateRandomCoords
	call getMapObj
	cmp bx, MapObjectType_None
	jne @@generateRandomEmptyCoords
	pop cx bx
	call setMapObj
	pop dx cx bx ax
	ret
endp

generateRandomCoords proc ; ; ax => coords
	push bx cx dx
	call readTimerCount
	mov cx, ax
	mov ah, 0
	mov al, cl
	mov dl, MapWidth
	xor al, ch
	add al, 37h
	mov ah, 0
	div dl
	mov bh, ah ; remainder
	mov dl, MapHeight
	mov ah, 0
	mov al, ch
	xor al, cl
	add al, 37h
	mov ah, 0
	div dl
	mov bl, ah
	mov ax, bx
	pop dx cx bx
	ret
endp

readTimerCount proc
	;pushf
	cli
	mov al, 00000000b    ; al = channel in bits 6 and 7, remaining bits clear
	out 43h, al        ; Send the latch command
	in al, 40h         ; al = low byte of count
	mov ah, al           ; ah = low byte of count
	in al, 40h         ; al = high byte of count
	rol ax, 8            ; al = low byte, ah = high byte (ax = current count)
	;popf
	ret
endp

decreaseExpirations proc
	push ax bx cx dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	
	call getMapObj
	cmp cx, Expires_Never
	je @@next
	dec cx
	cmp cx, 0
	jne @@justSet
	mov bx, MapObjectType_None
@@justSet:
	call setMapObj
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

changeSnakeDuration proc ; dx = duration change
	push ax bx cx dx
	neg dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	call getMapObj
	cmp bh, 0Ah
	jne @@notSnake
	cmp cx, dx
	jle @@remove
	sub cx, dx
	call setMapObj
	jmp @@next
@@remove:
	mov bx, MapObjectType_None
	mov cx, 0
	call setMapObj
@@notSnake:
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:

	pop dx cx bx ax
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