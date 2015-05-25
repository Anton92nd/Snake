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
		mov dl, '1'
		mov ah, 02h
		int 21h
		ret
@@failRet:
		mov ah, 02h
		mov dl, '0'
		int 21h
		ret
	
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
		mov dx, 01h
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
		
@@end:
		pop ax
		ret
@@initial:
		jmp @@end
@@digit:
		
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