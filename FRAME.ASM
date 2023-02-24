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

EXIT		macro

		mov ax, 4c00h
		int 21h

		endm

;=========================================================================
;=========================================================================
;=========================================================================


Start:		mov bx, 0b800h
		mov es, bx

	    	mov si, 82h             ;----------------------
	    	call GetNum             ; dx = 'x'
	    	push dx                 ;----------------------

					;----------------------
	    	call GetNum             ; dx = 'y'
					;----------------------
	
	    	mov di, dx              ;----------------------
	    	shl dx, 4               ; di = dx*80
	    	shl di, 6               ;
	   	add di, dx              ;
	   	pop dx                  ; di += 'x'
	    	add di, dx              ;----------------------

					;----------------------
	    	call GetNum             ; dx = width
	   	push dx                 ;----------------------

					;----------------------
	    	call GetNum             ; dx = height
	   	mov cx, dx              ;----------------------

					;----------------------
	    	call GetNum             ; dx = frame style
	    	push dx                 ;----------------------

					;----------------------
	    	call GetNum             ; dx = color
	    	add dx, offset COLORS   ;
	    	mov bx, dx              ;   COLOR PROCESSING
	    	mov ah, [bx]            ;----------------------


	   	pop dx                      ;----------------------
	    	push si                     ; user style ptr or msg ptr
	    	cmp dx, 9                   ;
	    	je USER_STYLE_PROCESSING    ;
					    ;   STYLE PROCESSING
	    	mov si, offset STYLES       ;
	    	mov bx, dx                  ;
	    	shl dx, 3                   ;   dx *= 9
	    	add dx, bx                  ;
	    	add si, dx                  ;
	    	pop bx                      ;
	    	pop dx                      ;
	    	push bx                     ; dx = width
					    ;----------------------

	    	jmp END_STYLE_PROCESSING     

USER_STYLE_PROCESSING:

            pop bx
            add bx, 9
            pop dx
            push bx 

END_STYLE_PROCESSING:
          
            push cx
            call DrawFrame
            pop cx
            shr cx, 1
            shl cx, 1
            add cx, 2

            pop si                      ; msg ptr
            
            shl di, 1                   ;----------------------
            shl dx, 1                   ;
            shr dx, 1                   ;
            sub di, dx                  ;
            mov bx, cx                  ;   DX = FRAME CENTER
            shl bx, 4                   ;;
            shl cx, 6                   ;; cx *= 80
            add cx, bx                  ;;
            sub di, cx                  ;----------------------

            mov bx, si                  ;----------------------
            sub bx, 81h                 ;   
            mov cl, ds:[80h]            ;   CX = MSG LEN
            mov ch, 0                   ;
            sub cx, bx                  ;----------------------

            sub di, cx
            shr di, 1
            shl di, 1

            call PrintMsg

	    EXIT

;=========================================================================
;=========================================================================
;=========================================================================
; Draw frame
;-------------------------------------------------------------------------
; Entry: AH = attr, CX = height, DX = width, SI = style_ptr, DI = position
; Exit : DI = DI + width*height
; Assum: ES = 0B800h
; Destr: AL, BX, SI, DI
;-------------------------------------------------------------------------

DrawFrame   proc

            mov bx, cx              ; save CX
            mov cx, dx              ; 

            call DrawLine

            mov cx, bx              ;return CX
            
            add di, 80              ;do offset to the
            sub di, dx              ;   next line

@@Next:     mov bx, cx              ;    <-------
            mov cx, dx              ;            |
            call DrawLine           ;            |
            mov cx, bx              ;            |  loop
            sub si, 3               ;            |
            add di, 80              ;            |
            sub di, dx              ;            |
            loop @@Next             ;    --------

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
; Destr: AL, CX, SI, DI 
;-------------------------------------------------------------------------

PrintMsg    proc

@@Next:     lodsb
            stosw
            loop @@Next

            ret
            endp

;=========================================================================



.data
STYLES      db  0DAh, 0C4h, 0BFh, 0B3h, ' ', 0B3h, 0C0h, 0C4h, 0D9h ;style 1
            db  0C9h, 0CDh, 0BBh, 0BAh, ' ', 0BAh, 0C8h, 0CDh, 0BCh ;style 2
            db  003d, 003d, 003d, 003d, ' ', 003d, 003d, 003d, 003d ;style 3

COLORS      db  05Eh, 04Eh      

end Start

