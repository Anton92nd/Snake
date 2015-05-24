model tiny
.386
.code
org 100h
locals @@

@entry:
		jmp @start
		call parseCommandLineArgs
@start:
		ret
	
;----------DFA states enum------------
WrongState db 00h
InitialState db 01h
SlashState db 02h
SymbolState db 03h
DigitState db 04h
;----------DFA states enum------------
Sygma equ 016d
;alphabet:/hlsf 0123456789
; indexes:0123456789abcdef

StateDFA struc
		_transitions db Sygma dup (0)
StateDFA ends

DFA StateDFA 4 dup (<>)
	
initDFA proc
		mov dx, type(StateDFA)
		lea si, DFA
		add si, dx
		;InitialState
		mov [si], cs:[SlashState]
		mov [si + 05h], [InitialState]
		add si, dx
		;SlashState
		mov [si + 01h], [SymbolState]
		
		ret
endp	
	
parseCommandLineArgs proc
		call initDFA
		ret
endp
		
end @entry