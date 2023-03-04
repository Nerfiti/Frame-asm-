.186
.model tiny
.code
org 100h
locals @@

;=========================================================================
; Exit
;-------------------------------------------------------------------------
; Entry: None
; Exit : N/A
; Destr: N/A
;-------------------------------------------------------------------------

EXIT        macro

            nop
	    mov ax, 4c00h
	    int 21h
            nop
            
	    endm

;=========================================================================
;=========================================================================
;=========================================================================


Start:	    mov bx, 0b800h
	    mov es, bx

            mov si, 82h             
                                        
            call GetNum                 ;----------------------
            push dx                     ; dx = frame style
                                        ;----------------------

                                        ;----------------------
            call GetNum                 ; dx = color
            add dx, offset COLORS       ;
            mov bx, dx                  ;   COLOR PROCESSING
            mov ah, [bx]                ;----------------------

                                        
            pop dx                      ;----------------------
            push si                     ; user style ptr or msg ptr
            cmp dx, 9                   ;
            je USER_STYLE_PROCESSING    ;
                                        ;   STYLE PROCESSING
STD_STYLE_PROCESSING:                   ;
                                        ;
            push dx                     ; save dx
            xor cx, cx                  ;
            mov cl, ds:[80h]            ;
            sub cx, si                  ;
            add cx, 81h                 ; get number of lines and max line len
            mov bh, ah                  ;
            call GetLines               ;
            mov ah, bh                  ;
            mov bx, dx                  ;
            pop dx                      ;
            push bx                     ; width
                                        ;
            mov si, offset STYLES       ;
            mov bx, dx                  ;
            shl dx, 3                   ;   dx *= 9
            add dx, bx                  ;
            add si, dx                  ;
            pop dx                      ;   dx = width
                                        ;----------------------
            
            jmp END_STYLE_PROCESSING     

USER_STYLE_PROCESSING:

            push si
            add si, 9
            xor cx, cx
            mov cl, ds:[80h]
            sub cx, si
            add cx, 81h
            mov bh, ah
            call GetLines
            mov ah, bh
            pop si
            pop dx
            push bx 

END_STYLE_PROCESSING:

            mov di, 23d     ;25d - 2d(frame up line and down line)
            sub di, cx      ;di now point to the end of the center line (*2/80)
            shr di, 1
            shl di, 1
            inc di

            mov bx, di      ;
            shl bx, 4       ; di *= 80
            shl di, 6       ;
            add di, bx      ;

            sub di, dx
            shr di, 1 

            add dx, 2
			call DrawFrame

            pop di
            pop si

            xor cx, cx                  
            mov cl, ds:[80h]            
            sub cx, si                  
            add cx, 81h

            call PrintMsg

	    EXIT

;=========================================================================
;=========================================================================
;=========================================================================
; Draw frame
;-------------------------------------------------------------------------
; Entry: AH = attr, CX = height (without up and down), DX = width, SI = style_ptr, DI = position
; Exit : DI = DI + width*height, push ptr to start space in the frame 
; Assum: ES = 0B800h
; Destr: AL, BX, SI, DI
;-------------------------------------------------------------------------

DrawFrame   proc

            mov bx, cx              ; save CX
            mov cx, dx              ; 

            call DrawLine

            mov cx, bx              ;return CX
            
            add di, 80d             ;do offset to the
            sub di, dx              ;   next line

            cmp cx, 0
            je @@LOOP_END

            inc di
            pop bx
            push di
            push bx
            dec di

@@Next:     mov bx, cx              ;    <-------
            mov cx, dx              ;            |
            call DrawLine           ;            |
            mov cx, bx              ;            |  loop
            sub si, 3               ;            |
            add di, 80              ;            |
            sub di, dx              ;            |
            loop @@Next             ;    --------

@@LOOP_END:
            add si, 3
            mov bx, cx
            mov cx, dx
            call DrawLine
            mov cx, bx

	    ret
	    endp

;=========================================================================
; Draw line
;-------------------------------------------------------------------------
; Entry: AH = attr, SI = style_ptr, CX = width, DI = position (x2 for attr)
; Exit : DI = DI + CX, CX = 0
; Assum: ES = 0B800h
; Destr: AL, CX, DI, SI
;-------------------------------------------------------------------------

DrawLine    proc
            
            shl di, 1

            lodsb
            stosw

            sub cx, 2
            lodsb
            rep stosw

            lodsb
            stosw

            shr di, 1

	    ret
	    endp

;=========================================================================
; Get number from a string (until sym ' ')
;-------------------------------------------------------------------------
; Entry: SI = str ptr
; Exit : DX = number, SI points to the symbol after ' ' after number
; Destr: AL, BX, DX, SI
;-------------------------------------------------------------------------

GetNum      proc

            xor dx, dx              ; clear dx
            xor al, al              ; clear al

            lodsb

@@convert:  cmp al, '0'
            jb @@end_convert
            cmp al, '9'
            ja @@end_convert

            mov bx, dx              ;
            shl dx, 2               ;
            add dx, bx              ; dx *= 10
            shl dx, 1               ;
            
            sub al, '0'             ;
            add dl, al              ; dx += al - '0'
            adc dx, 0               ;
            
            lodsb
            jmp @@convert


@@end_convert:
            ret
            endp

;=========================================================================
; Print message in the frame
;-------------------------------------------------------------------------
; Entry: AH = attr, CX = msg len, SI = str ptr, DI = dest ptr
; Exit : SI += msg len, DI += 2*msg len
; Destr: AL, BX, CX, SI, DI 
;-------------------------------------------------------------------------

PrintMsg    proc

            xor bx, bx
            shl di, 1

@@Next:     lodsb
            cmp al, '$'
            je @@NEW_LINE

            add bx, 2
            stosw
            loop @@Next
            jmp @@END

@@NEW_LINE: add di, 160d
            sub di, bx
            xor bx, bx
            loop @@Next

@@END:

            ret
            endp

;=========================================================================
; Get number of lines and max line length
;-------------------------------------------------------------------------
; Entry: CX = msg len, DS:SI = str ptr
; Exit : CX = number of lines, DX = max line length, SI += msg len
; Destr: AX, CX, DX, SI 
;-------------------------------------------------------------------------

GetLines    proc

            xor dx, dx
            xor ax, ax
            cmp cx, 0
            je @@EXIT

@@Next:     dec cx
            lodsb           ;load sym
            inc dl          ;inc current line len
            cmp al, '$'     
            je @@NEW_LINE
            cmp cx, 0
            je @@END
            jmp @@Next      ; if it isn't a new line

@@NEW_LINE: inc ah          ;line counter
            cmp dl, dh      
            jb @@NOT_MAX    
            mov dh, dl      ;set max

@@NOT_MAX:  xor dl, dl      ;continue    
            cmp cx, 0
            je @@END
            jmp @@Next      

@@END:      cmp dl, dh
            jb @@NOT_MAX_END
            mov dh, dl
@@NOT_MAX_END:
            inc ah          ;last line
            mov cl, ah      
            mov ch, 0       
            mov dl, dh
            mov dh, 0

@@EXIT:
            ret
            endp

;=========================================================================



.data
STYLES      db  0DAh, 0C4h, 0BFh, 0B3h, ' ', 0B3h, 0C0h, 0C4h, 0D9h ;style 1
            db  0C9h, 0CDh, 0BBh, 0BAh, ' ', 0BAh, 0C8h, 0CDh, 0BCh ;style 2
            db  003d, 003d, 003d, 003d, ' ', 003d, 003d, 003d, 003d ;style 3

COLORS      db  05Eh, 04Eh      

end Start

