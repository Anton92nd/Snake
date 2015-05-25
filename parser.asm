model tiny
.386
.code
org 100h
locals @@

@entry:
		jmp @start
@start:
		call parseCommandLineArgs
		ret
	
;----------DFA states enum------------
WrongState db 00h
InitialState db 01h
SlashState db 02h
SymbolState db 03h
DigitState db 04h
;----------DFA states enum------------
StatesCount equ 05d
KeysCount equ 04h
KeyValues db KeysCount dup (0)
LastKey db 05h
Sygma equ 016d
Alphabet db '/hlsf 0123456789'
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
		
		mov di, si					;by digits
		add di, KeysCount + 02h
		mov al, [DigitState]
		mov cx, 0ah
		rep stosb
		
		add si, Sygma
		;SlashState
		mov di, si					;by keys
		inc di
		mov cx, KeysCount
		mov al, [SymbolState]
		rep stosb
		
		add si, Sygma
		;SymbolState
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
		mov [si], al				;q0, q2, q3
		mov [si + 02h], al
		mov [si + 03h], al
		ret
endp

parseArgs proc
		lea di, DFA
		add di, Sygma
		xor dx, dx
		mov si, 80h
		xor cx, cx
		mov cl, [si]
		inc si
@@loop:
		lodsb
		call traslateSymbol
		cmp al, 0FFh
		je @@endFail
		call processSymbol
		cmp al, Sygma
		je @@endFail
		call getDFAstate
		mov dx, ax
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

processSymbol proc ;al <- Sygma if wrong
		push ax
		test al, al
		jz @@end
		cmp al, KeysCount + 01h
		je @@end
		jg @@digit
		dec al
		mov [LastKey], al
@@end:
		pop ax
		ret
@@digit:
		cmp [LastKey], KeysCount
		jge @@endFail
		push dx
		xor dx, dx
		mov dl, al
		sub dx, KeysCount + 02h
		xor bx, bx
		mov bl, [LastKey]
		add bx, offset KeyValues
		mov al, [bx]
		mov ah, 0ah
		mul ah
		add ax, dx
		pop dx
		mov [bx], al
		jmp @@end
@@endFail:
		pop ax
		mov al, Sygma
		ret
endp

getDFAstate proc
		push bx
		mov bx, ax
		add bx, di
		mov al, [bx]
		pop bx
		ret
endp
	
parseCommandLineArgs proc
		call initDFA
		call parseArgs
		ret
endp
		
end @entry