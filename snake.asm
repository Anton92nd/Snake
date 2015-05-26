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
		call parseCommandLineArgs
		cmp al, 0FFh
		je @@failRet
		call checkKeysValues
		cmp al, 0FFh
		je @@failRet
		mov al, [KeyValues]
		test al, al
		jz @@argsParsed
		call showHelp
@@failRet:
		ret
@@argsParsed:
		call fillArgs
		call initialize
		call makeSnake
		call drawMap
		mov [GameStatus], Game_PausedPause
@@loop:
		mov ax, 0h
		mov es, ax
		mov cx, [Speed]
		add cx, [word ptr es:046ch]
@@wait:
		call offSound
		cmp [GameStatus], Game_Over
		je @@end
		hlt
		cmp cx, [word ptr es:046ch]
		jne @@wait
		cmp [GameStatus], Game_Running
		jne @@paused
		call makeTurn
		inc [GameTurns]
		call drawMap
		jmp @@loop
@@paused:
		cmp [GameStatus], Game_PausedOver
		je @@pausedOver
		cmp [GameStatus], Game_PausedHelp
		je @@pausedHelp
		call printPaused
		jmp @@pauseLoop
@@pausedOver:
		call printGameover
		call playTune
		jmp @@pauseLoop
@@pausedHelp:
		call printHelp
@@pauseLoop:
		mov dl, [GameStatus]
@@whilePausedStay:
		cmp [GameStatus], Game_Over
		je @@end
		cmp dl, [GameStatus]
		je @@whilePausedStay
		jmp @@loop
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
		je @@escape
		cmp al, 03bh
		je @@helpKey
		cmp al, 01ch
		je @@enterKey
		cmp al, 48h
		je @@ifUp
		cmp al, 4dh
		je @@ifRight
		cmp al, 50h
		je @@ifDown
		cmp al, 4bh
		je @@ifLeft
		cmp al, 0dh
		je @@ifPlus
		cmp al, 0ch
		je @@ifMinus
		cmp al, 4eh
		je @@ifPlus
		cmp al, 4ah
		je @@ifMinus
		jmp @@end
@@escape:
		cmp [GameStatus], Game_Running
		je @@setPause
		mov [GameStatus], Game_Over
		jmp @@end
@@setPause:
		mov [GameStatus], Game_PausedPause
		jmp @@end
@@helpKey:
		mov [GameStatus], Game_PausedHelp
		jmp @@end
@@enterKey:
		cmp [GameStatus], Game_PausedOver
		je @@setOver
		cmp [GameStatus], Game_Running
		je @@setPause
		mov [GameStatus], Game_Running
		jmp @@end
@@setOver:
		mov [GameStatus], Game_Over
		jmp @@end
@@ifUp:
		mov bx, [HeadType]
		cmp bx, MapObjectType_SnakePartDown
		je @@reverse
		mov ax, [HeadCoords]
		call getMapObj
		mov bx, MapObjectType_SnakePartUp
		call setMapObj
		jmp @@end
@@ifDown:
		mov bx, [HeadType]
		cmp bx, MapObjectType_SnakePartUp
		je @@reverse
		mov ax, [HeadCoords]
		call getMapObj
		mov bx, MapObjectType_SnakePartDown
		call setMapObj
		jmp @@end
@@ifLeft:
		mov bx, [HeadType]
		cmp bx, MapObjectType_SnakePartRight
		je @@reverse
		mov ax, [HeadCoords]
		call getMapObj
		mov bx, MapObjectType_SnakePartLeft
		call setMapObj
		jmp @@end
@@ifRight:
		mov bx, [HeadType]
		cmp bx, MapObjectType_SnakePartLeft
		je @@reverse
		mov ax, [HeadCoords]
		call getMapObj
		mov bx, MapObjectType_SnakePartRight
		call setMapObj
		jmp @@end
@@reverse:
		call reverseSnake
		jmp @@end
@@ifMinus:
		inc [Speed]
		jmp @@end
@@ifPlus:
		cmp [Speed], 1
		je @@end
		dec [Speed]
		jmp @@end
@@end:
		mov al, 20h ;Send EOI (end of interrupt)
		out 20h, al ; to the 8259A PIC.
		pop dx cx bx ax
		iret
endp

showHelp proc
		mov ah, 09h
		lea dx, HelpCmdText
		int 21h
		ret
endp

HelpCmdText db 'Usage: snake [optional keys]', 13, 10,'	/? - show help', 13, 10,'	/l <1..9> - starting snake length', 13, 10,'	/f <1..20> - number of food pieces', 13, 10,'	/s <0..2> - self-collision behaviour', 13, 10,'	  0 - death', 13, 10,'	  1 - override', 13, 10,'	  2 - cutting tail$'

fillArgs proc
		mov al, [KeyValues + 02h]
		mov [SelfCollisionAllowed], al
		mov al, [KeyValues + 01h]
		mov [StartSnakeLength], al
		mov al, [KeyValues + 03h]
		mov [Food1Count], al
		ret
endp

MaxSnakeStartLength equ 09h
StartSnakeLength db ?
Food1Count db 01h
		
checkKeysValues proc
		xor ax, ax
		mov al, [KeyValues + 01h]
		cmp ax, MaxSnakeStartLength
		jg @@endFail
		xor al, al
		cmp al, [KeyValues + 01h]
		jne @@skipLength
		mov al, 04h
		mov [KeyValues + 01h], al
@@skipLength:
		mov al, [KeyValues + 02h]
		cmp al, 02h
		jg @@endFail
		mov al, 02h
		cmp [KeyValues + 02h], al
		jne @@skipCollision
		mov al, 03h
		mov [KeyValues + 02h], al
@@skipCollision:
		xor al, al
		cmp [KeyValues + 03h], al
		jne @@skipFood
		mov al, 01h
		mov [KeyValues + 03h], al
@@skipFood:
		ret
@@endFail:
		mov al, 0FFh
		ret
endp
	
;----------DFA states enum------------
WrongState db 00h
InitialState db 01h
SlashState db 02h
KeyWithValState db 03h
KeyOnlyState db 04h
DigitState db 05h
;----------DFA states enum------------
StatesCount equ 06d
KeysCount equ 04h
KeyValues db KeysCount dup (0)
LastKey db 05h
Sygma equ 016d
Alphabet db '/?lsf 0123456789'
Terminal db StatesCount dup (0)


traslateSymbol proc
		push si cx
		lea si, Alphabet
		mov cx, Sygma
@@whileAlNotEqualToCsSi:
		cmp al, [si]
		je @@found
		inc si
		loop @@whileAlNotEqualToCsSi
		mov al, 0FFh
		jmp @@end
@@found:
		sub si, offset Alphabet
		mov ax, si
@@end:
		pop cx si
		ret
endp

DFA db Sygma * StatesCount dup (0)
	
initDFA proc
		xor ax, ax
		cld
		lea si, DFA
		add si, Sygma
		;InitialState
		
		mov al, [SlashState]		;by slash
		mov [si], al
		
		mov al, [InitialState]		;by whitespace
		mov [si + 05h], al
		
		add si, Sygma
		;SlashState
		mov al, [KeyOnlyState]
		mov [si + 01h], al
		
		mov di, si					;by keys with values
		add di, 02h
		mov cx, KeysCount - 01h
		mov al, [KeyWithValState]
		rep stosb
		
		add si, Sygma
		;KeyWithValState
		mov al, [KeyWithValState]	;by whitespace
		mov [si + KeysCount + 01h], al
		
		mov di, si					;by digits
		add di, KeysCount + 02h
		mov al, [DigitState]
		mov cx, 0ah
		rep stosb 
		
		add si, Sygma
		;KeyOnlyState
		mov al, [InitialState]
		mov [si + KeysCount + 01h], al
		
		add si, Sygma
		;DigitState
		mov al, [InitialState]		;by whitespace
		mov [si + KeysCount + 01h], al
		
		mov di, si					;by digits
		add di, KeysCount + 02h
		mov al, [DigitState]
		mov cx, 0ah
		rep stosb 
		;
		lea si, Terminal
		mov al, 01h
		mov [si + 01h], al				;q1, q4, q5
		mov [si + 04h], al
		mov [si + 05h], al
		ret
endp

parseArgs proc
		lea di, DFA
		add di, Sygma
		xor dx, dx
		mov dl, 01h
		mov si, 80h
		xor cx, cx
		mov cl, [si]
		test cl, cl
		jz @@end
		inc si
@@loop:
		lodsb
		call traslateSymbol
		cmp al, 0FFh
		je @@endFail
		call processSymbol ;
		call getDFAstate ;to dx
		mov al, dl
		mov ah, Sygma
		mul ah
		lea di, DFA
		add di, ax
		loop @@loop
@@end:
		lea bx, Terminal
		add bx, dx
		mov al, [bx]
		test al, al
		jz @@endFail
		ret
@@endFail:
		mov al, 0FFh
		ret
endp

processSymbol proc ;al = symbol dx = state ;al <- Sygma if wrong
		push ax bx cx
		test al, al				;slash
		jz @@end
		cmp al, KeysCount + 01h	;whitespace
		je @@end
		cmp al, KeysCount
		jg @@digit
		cmp al, 01h
		je @@help
		dec al
		mov [LastKey], al
		jmp @@end
@@digit:
		mov cx, ax
		sub cx, KeysCount + 02h
		xor bx, bx
		mov bl, [LastKey]
		add bx, offset KeyValues
		mov al, [bx]
		mov ah, 0Ah
		mul ah
		add ax, cx
		mov [bx], al
		jmp @@end
@@help:
		mov [KeyValues], al
@@end:
		pop cx bx ax
		ret
endp

getDFAstate proc
		push bx
		xor dx, dx
		mov bx, ax
		add bx, di
		mov dl, [bx]
		pop bx
		ret
endp
	
parseCommandLineArgs proc
		call initDFA
		call parseArgs
		ret
endp

printHelp proc
		push bx dx
		mov dx, 0207h
		lea bx, HelpTextLine1
		call printLine
		inc dh
		lea bx, HelpTextline2
		call printLine
		inc dh
		lea bx, HelpTextLine3
		call printLine
		inc dh
		lea bx, HelpTextLine4
		call printLine
		inc dh
		lea bx, HelpTextLine5
		call printLine
		pop dx bx
		ret
endp

printPaused proc
		push bx dx
		mov dx, 0209h
		lea bx, PauseTextLine1
		call printLine
		inc dh
		lea bx, PauseTextline2
		call printLine
		inc dh
		lea bx, PauseTextLine3
		call printLine
		pop dx bx
		ret
endp


fillNumber proc 	;bx = offset to end of insert, cx = number
		push ax bx cx dx di
		xor dx, dx
		mov ax, cx
		mov cx, 03h
		mov di, 0Ah
@@loop:
		div di
		add dx, '0'
		mov [bx], dl
		dec bx
		xor dx, dx
		loop @@loop
		pop di dx cx bx ax
		ret
endp

printGameover proc
		push ax bx cx dx
		xor cx, cx
		mov ax, [HeadCoords]
		call getMapObj
		mov [SnakeLengthAtFinish], cl
		mov dx, 020fh
		lea bx, EndTextLine1
		call printLine
		xor cx, cx
		mov bx, offset EndTextLine2 + 009d
		mov cl, [Food1Eaten]
		call fillNumber
		mov bx, offset EndTextLine2 + 020d
		mov cl, [Food2Eaten]
		call fillNumber
		mov dx, 0407h
		lea bx, EndTextLine2
		call printLine
		inc dh
		mov bx, offset EndTextLine3 + 009d
		mov cl, [GameTurns]
		call fillNumber
		lea bx, EndTextLine3
		call printLine
		inc dh
		mov bx, offset EndTextLine4 + 010d
		mov cl, [SnakeLengthAtFinish]
		call fillNumber
		lea bx, EndTextLine4
		call printLine
		inc dh
		lea bx, EndTextline5
		call printLine
		pop dx cx bx ax
		ret
endp

PauseTextLine1 db 'PAUSED$'
PauseTextLine2 db '[Press Enter to resume]$'
PauseTextLine3 db '[Press Escape to exit]$'

HelpTextLine1 db 'Use arrow keys to turn snake$'
HelpTextLine2 db 'Press Enter to pause/unpause$'
HelpTextLine3 db 'Use +/- to change speed $'
HelpTextLine4 db 'Try "/?" key to see keys$'
HelpTextLine5 db '[Press Enter to resume]$'

EndTextLine1 db 'GAME OVER$'
EndTextLine2 db 'Food1: ___	Food2: ___$'
EndTextLine3 db 'Turns: ___$' 
EndTextLine4 db 'Length: ___$'
EndTextLine5 db '[Press Enter/Escape to exit]$'
		
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
MapObjectType_Wall4 equ 0204h
MapObjectType_SnakePartLeft equ 0A00h
MapObjectType_SnakePartUp equ 0A01h
MapObjectType_SnakePartRight equ 0A02h
MapObjectType_SnakePartDown equ 0A03h
;------------Enum object type------------
Expires_Never equ 0
Food3Expires dw 0fh
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

;-------------Game status enum---------------
Game_Running equ 0h
Game_PausedPause equ 10h
Game_PausedHelp equ 11h
Game_PausedOver equ 12h
Game_Over equ 20h
;-------------Game status enum---------------

;-------------Game statistics----------------
Food1Eaten db 0h
Food2Eaten db 0h
GameTurns db 0h
SnakeLengthAtFinish db 0h
;-------------Game statistics----------------


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
	Food3Color dw 41d
	Wall1Color dw 240d
	Wall2Color dw 11d
	Wall3Color dw 80d
	Wall4Color dw 32d
	NoneColor  dw 0d
;----------Colors----------
SnakeColorsCount equ 05h
SnakeColors dw 15d, 11d, 13d, 45d, 92d
CurrentSnakeColor db 0

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
		jne @@checkIfFood3
		mov dx, [Food2Color]
		call drawBox
		jmp @@end
@@checkIfFood3:
		cmp bx, MapObjectType_Food3
		jne @@checkIfWall1
		mov dx, [Food3Color]
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
		jne @@checkIfWall4
		mov dx, [Wall3Color]
		call drawBox
		jmp @@end
@@checkIfWall4:
		cmp bx, MapObjectType_Wall4
		jne @@ifNone
		mov dx, [Wall4Color]
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
		cmp al, 0h
		je @@isWall4
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
@@isWall4:
		mov bx, MapObjectType_Wall4
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
		mov ax, 0a07h
		mov [HeadCoords], ax
		mov [HeadType], MapObjectType_SnakePartRight
		mov cl, [StartSnakeLength]
		xor ch, ch
@@loop:
		mov bx, MapObjectType_SnakePartRight
		call setMapObj
		dec ah
		dec cx
		jcxz @@end
		jmp @@loop
@@end:
		mov ax, 0B0Ah
		mov bx, MapObjectType_Food1
		mov cx, Expires_Never
		call setMapObj
		mov ax, 100Ah
		mov bx, MapObjectType_Food2
		mov cx, Expires_Never
		call setMapObj
		mov ax, 150Bh
		mov bx, MapObjectType_Food3
		mov cx, [Food3Expires]
		call setMapObj
		call setRemainingFood
		
		pop dx cx bx ax
		ret
endp

setRemainingFood proc
		push ax bx cx dx
		xor cx, cx
		mov cl, [Food1Count]
		dec cx
		test cx, cx
		jz @@end
@@loop:
		push cx
		xor cx, cx
		mov bx, MapObjectType_Food1
		call generateMapObjWhereEmpty
		mov bx, MapObjectType_Food2
		call generateMapObjWhereEmpty
		pop cx
		loop @@loop
@@end:
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
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@alive:
		cmp cx, [MaxSnakeLength]
		jb @@notWonYet 
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@notWonYet:
		call getNextAx
		call getMapObj
		mov dx, bx
		call onCollideWith
		cmp di, 1
		je @@collide
		cmp [GameStatus], Game_PausedOver
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


SelfCollisionAllowed db 3

onCollideWith proc ; ax = with who, dx = target type; di => isRecheckNeeded
		push ax bx cx dx
		mov di, 0
		cmp dh, 0Ah
		je @@withUrSelf
		cmp dx, MapObjectType_Food1
		je @@withFood1
		cmp dx, MapObjectType_Food2
		je @@withFood2
		cmp dx, MapObjectType_Food3
		je @@withFood3
		cmp dx, MapObjectType_Wall1
		je @@withWall1
		cmp dx, MapObjectType_Wall2
		je @@withWall2
		cmp dx, MapObjectType_Wall3
		je @@withWall3
		cmp dx, MapObjectType_Wall4
		je @@withWall4
		jmp @@end
@@withUrSelf:
		mov dl, [SelfCollisionAllowed]
		and dl, 03h
		cmp dl, 03h
		je @@removeTail
		test [SelfCollisionAllowed], 01h
		jnz @@end
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@withFood1:
		inc [Food1Eaten]
		mov bx, Note_C2
		call playSoundFromBx
		mov dx, 1
		call changeSnakeDuration
		mov bx, MapObjectType_Food1
		mov cx, Expires_Never
		call generateMapObjWhereEmpty
		jmp @@end
@@withFood2:
		inc [Food2Eaten]
		mov bx, Note_D2
		call playSoundFromBx
		mov dx, -2
		call changeSnakeDuration
		mov bx, MapObjectType_Food2
		mov cx, Expires_Never
		call generateMapObjWhereEmpty
		jmp @@end
@@withFood3:
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@withWall1:
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@withWall2:
		mov bx, Note_E
		call playSoundFromBx
		call reverseSnake
		mov di, 1
		jmp @@end
@@withWall3:
		mov bx, Note_H
		call playSoundFromBx
		jmp @@end
@@withWall4:
		mov bx, Note_H
		call playSoundFromBx
		call rollSnakeColor
		mov ax, [HeadCoords]
		call getMapObj
		mov dx, cx
		cmp dx, 01h
		je @@die
		shr dx, 1
		call decreaseExpirationsByDx
		call reverseSnake
		mov di, 1
		jmp @@end
@@removeTail:
		mov bx, Note_Fs
		call playSoundFromBx
		mov dx, cx
		call decreaseExpirationsByDx
		jmp @@end
@@die:
		mov [GameStatus], Game_PausedOver
		jmp @@end
@@end:
		pop dx cx bx ax
		ret
endp

rollSnakeColor proc
		push ax bx
		xor bh, bh
		mov bl, [CurrentSnakeColor]
		inc bl
		cmp bx, SnakeColorsCount
		jne @@skip
		xor bl, bl
@@skip:
		mov [CurrentSnakeColor], bl
		add bx, bx
		add bx, offset SnakeColors
		mov ax, [bx]
		mov [SnakeColor], ax
		pop bx ax
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
		;cli
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
		jne @@set
		cmp bx, MapObjectType_Food3
		je @@placeNewFood3
		mov bx, MapObjectType_None
		jmp @@set
@@placeNewFood3:
		call SetNewFood3
		mov bx, MapObjectType_None
@@set:
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

decreaseExpirationsByDx proc
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
		cmp bx, MapObjectType_Food3
		je @@next
		cmp cx, Expires_Never
		je @@next
		cmp cx, dx
		jle @@toZero
		sub cx, dx
		jmp @@set
@@toZero:
		mov cx, Expires_Never
		mov bx, MapObjectType_None
@@set:
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

SetNewFood3 proc
		push bx cx
		mov bx, MapObjectType_Food3
		mov cx, [Food3Expires]
		call generateMapObjWhereEmpty
		pop cx bx
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

printLine proc ; bx = word off, dh = y, dl = x
	push ax bx cx dx
	push bx
	mov bx, 0
	mov ah, 2
	int 10h
	pop dx
	mov ah, 9
	int 21h
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

offSound proc
	push ax
	in      al, 61h
	and     al, 11111100b
	out     61h, al
	pop ax
	ret
endp

playNote proc ; bx = freq or 0 if silence, cx = length
	push ax bx cx dx es
	mov ax, 0
	mov es, ax
	add cx, [word ptr es:046Ch]
	cmp bx, 0
	je delay
	call playSoundFromBx
delay:
	cmp [word ptr es:046Ch], cx
	jne delay
	
	call offSound
	
	pop es dx cx bx ax
	ret
endp

TunePlayed db 0
playTune proc
	push ax bx cx dx
	cmp [TunePlayed], 1
	je @@end
	mov bx, Note_C
	mov cx, 06h
	call playNote
	mov bx, 0h
	mov cx, 04h
	call playNote
	mov bx, Note_C
	mov cx, 06h
	call playNote
	mov bx, 0h
	mov cx, 04h
	call playNote
	mov bx, Note_C
	mov cx, 02h
	call playNote
	mov bx, 0
	mov cx, 02h
	call playNote
	mov bx, Note_C
	mov cx, 04h
	call playNote
	mov bx, 0h
	mov cx, 08h
	call playNote
	mov bx, Note_E
	mov cx, 04h
	call playNote
	mov bx, 0h
	mov cx, 04h
	call playNote
	mov bx, Note_D
	mov cx, 02h
	call playNote
	mov bx, 0h
	mov cx, 02h
	call playNote
	mov bx, Note_D
	mov cx, 04h
	call playNote
	mov bx, 0h
	mov cx, 04h
	call playNote
	mov bx, Note_C
	mov cx, 02h
	call playNote
	mov bx, 0h
	mov cx, 02h
	call playNote
	mov bx, Note_C
	mov cx, 04h
	call playNote
	mov bx, 0h
	mov cx, 04h
	call playNote
	mov bx, Note_C
	mov cx, 02h
	call playNote
	mov bx, 0h
	mov cx, 02h
	call playNote
	mov bx, Note_C
	mov cx, 08h
	call playNote
@@end:
	pop dx cx bx ax
	ret
endp

Note_C equ 9121d
Note_Cs equ 8609d
Note_D equ 8126d
Note_Ds equ 7670d
Note_E equ 7239d
Note_F equ 6833d
Note_Fs equ 6449d
Note_G equ 6087d
Note_Gs equ 5746d
Note_A equ 5424d
Note_B equ 5120d
Note_H equ 4832d
Note_C2 equ 4560d
Note_Cs2 equ 8609d / 2
Note_D2 equ 8126d / 2
Note_Ds2 equ 7670d / 2
Note_E2 equ 7239d / 2
Note_F2 equ 6833d / 2
Note_Fs2 equ 6449d / 2
Note_G2 equ 6087d / 2
Note_D3 equ Note_D2 / 2
Note_F6 equ Note_F2 / 16
Note_E5 equ Note_E2 / 8

end @entry