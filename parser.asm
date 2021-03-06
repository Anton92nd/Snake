model tiny
.386
.code
org 100h
locals @@

@entry:
		jmp @start
@start:
		call parseCommandLineArgs
		cmp al, 0FFh
		je @@failRet
		call checkKeysValues
		cmp al, 0FFh
		je @@failRet
		mov ah, 02h
		mov dl, [KeyValues + 01h]
		add dl, '0'
		int 21h
		mov dl, ' '
		int 21h
		mov dl, [KeyValues + 02h]
		add dl, '0'
		int 21h
		mov dl, ' '
		int 21h
		mov dl, [KeyValues + 03h]
		add dl, '0'
		int 21h
		ret
@@failRet:
		mov ah, 02h
		mov dl, '!'
		int 21h
		ret

MaxSnakeStartLength equ 09h
		
checkKeysValues proc
		xor ax, ax
		mov al, [KeyValues + 01h]
		cmp ax, MaxSnakeStartLength
		jg @@endFail
		xor al, al
		cmp al, [KeyValues + 01h]
		jne @@skipLength
		mov al, 03h
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
		mov [bx], ax
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
		
end @entry