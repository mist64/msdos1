;-----------------------------------------------------------------------------
; DOS 1.0 IBMBIO.COM (disk image MD5 73c919cecadf002a7124b7e8bfe3b5ba)
;   http://www.pagetable.com/
;-----------------------------------------------------------------------------


SECTOR_SIZE     equ     0x0200          ; size of a sector
DOS_SIZE        equ     10000           ; max size of IBMDOS.COM in bytes
PAUSE_KEY       equ     0x7200          ; scancode + charcode of PAUSE key
KEYBUF_NEXT     equ     0x041A          ; next character in keyboard buffer
KEYBUF_FREE     equ     0x041C          ; next free slot in keyboard buffer
KEYBUF          equ     0x041E          ; keyboard buffer data
LOGICAL_DRIVE   equ     0x0504          ; linear address of logical drive byte
SEG_DOS_TEMP    equ     0xE0            ; segment in which DOS was loaded
SEG_DOS         equ     0xB1            ; segment in which DOS will run
SEG_BIO         equ     0x60            ; segment in which BIO is running


;-----------------------------------------------------------------------------


                org 0x0000              ; segment 0x0060


                jmp     INIT            ; 0x0060:0x0000 entry point
                jmp     STATUS          ; 0x0060:0x0003 check for keypress
                jmp     INP             ; 0x0060:0x0006 get key from keyboard
                jmp     OUTP            ; 0x0060:0x0009 send character to screen
                jmp     PRINT           ; 0x0060:0x000C send character to printer
                jmp     AUXIN           ; 0x0060:0x000F get character from serial
                jmp     AUXOUT          ; 0x0060:0x0012 send character to serial
                jmp     READ            ; 0x0060:0x0015 read sector(s) from disk (INT 0x25)
                jmp     WRITE           ; 0x0060:0x0018 write sector(s) to disk  (INT 0x26)
                jmp     DSKCHG          ; 0x0060:0x001B check for disk change


;-----------------------------------------------------------------------------


                dw SEG_DOS              ; ???
                dw TXT_VERSION          ; ???
TXT_VERSION     db 'BIOS Version 1.00'
                db ' '+0x80
                db '22-Jul-81',0


;-----------------------------------------------------------------------------


ERR_PAPER       db 13,10,'Out of pape','r'+0x80,13,10,0
ERR_PRINTER     db 13,10,'Printer faul','t'+0x80,13,10,0
ERR_AUX         db 13,10,'Aux I/O erro','r'+0x80,13,10,0


;-----------------------------------------------------------------------------
; check for keypress
;  AL = character
;  Z  = set if no character
;  all other registers preserved
;-----------------------------------------------------------------------------
STATUS          mov     al, [cs:next_char]; check for waiting character
                or      al, al
                jnz     char_avail      ; yes, return it
                push    dx
                xchg    ax, dx
                mov     ah, 1
                int     0x16            ; otherwise get key (don't clear)
                jz      status_exit     ; no key
                cmp     ax, PAUSE_KEY   ; PAUSE key?
                jnz     status_exit
                mov     al, 0x10        ; convert into Ctrl+P
                or      al, al


status_exit     mov     ah, dh          ; restore original AH
                pop     dx
char_avail      retf


;-----------------------------------------------------------------------------
; Interrupt 0x1B handler: Control+Break handler
;-----------------------------------------------------------------------------
int_1B          mov     byte [cs:next_char], 3; put code for Ctrl+C
                iret                    ; into keyboard queue


;-----------------------------------------------------------------------------
; Interrupt 0x00 handler: Division by Zero
;-----------------------------------------------------------------------------
int_00          sti
                push    ax
                push    dx
                mov     dx, ERR_DIVIDE
                call    print_string
                pop     dx
                pop     ax
                int     0x23            ; exit program through Ctrl+C path


;-----------------------------------------------------------------------------
; Interrupt 0x00 handler: Single Step
; Interrupt 0x03 handler: Breakpoint
; Interrupt 0x04 handler: Overflow
;-----------------------------------------------------------------------------
iret1           iret                    ; empty interrupt handler


;-----------------------------------------------------------------------------


ERR_DIVIDE      db 13,10,'Divide overflo','w'+0x80,13,10,0


;-----------------------------------------------------------------------------
; get key from keyboard
;  AL = character
;  all other registers preserved
;-----------------------------------------------------------------------------
again           xchg    ax, dx
                pop     dx
INP             mov     al, 0
                xchg    al, [cs:next_char]; get and clear waiting character
                or      al, al
                jnz     inp_exit        ; there is no character waiting
                push    dx
                xchg    ax, dx
                mov     ah, 0
                int     0x16            ; then read character from keyboard
                or      ax, ax
                jz      again
                cmp     ax, PAUSE_KEY
                jnz     not_pause2
                mov     al, 0x10        ; Ctrl+P
not_pause2      cmp     al, 0
                jnz     skip1           ; key with ASCII representation
                mov     [cs:next_char], ah; return scancode next time
skip1           mov     ah, dh          ; restore AH
                pop     dx
inp_exit        retf


;-----------------------------------------------------------------------------
; send character to screen
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
OUTP            push    bp
                push    ax
                push    bx
                push    si
                push    di
                mov     ah, 0x0E
                cs                      ; XXX makes no sense
                mov     bx, 7
                int     0x10            ; print character
                pop     di
                pop     si
                pop     bx
                pop     ax
                pop     bp
                retf


;-----------------------------------------------------------------------------
; send character to printer
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
PRINT           push    ax
                push    dx
                mov     byte [cs:printer_retry], 0
printer_again   mov     dx, 0           ; printer port #0
                mov     ah, 0
                int     0x17            ; send character to printer
                mov     dx, ERR_PAPER
                test    ah, 0x20
                jnz     printer_error   ; out of paper error
                mov     dx, ERR_PRINTER
                test    ah, 5
                jz      pop_dx_ax_retf  ; no timeout error, return
                xor     byte [cs:printer_retry], 1
                jnz     printer_again   ; on a timeout, try twice
printer_error   call    print_string
pop_dx_ax_retf  pop     dx
                pop     ax
                retf


;-----------------------------------------------------------------------------
; print zero-terminated string at DS:DX
;-----------------------------------------------------------------------------
print_string    xchg    si, dx
prints1         cs lodsb
                and     al, 0x7F        ; clear bit 7 (XXX why?)
                jz      prints2         ; zero-terminated
                call    SEG_BIO:OUTP    ; print character
                jmp     prints1         ; loop
prints2         xchg    si, dx
                retn


;-----------------------------------------------------------------------------
; get character from serial
;  AL = character
;  all other registers preserved
;-----------------------------------------------------------------------------
AUXIN           push    dx
                push    ax
                mov     dx, 0           ; serial port #0
                mov     ah, 2
                int     0x14            ; get character from serial port
                mov     dx, ERR_AUX
                test    ah, 0x0E        ; framing, parity or overrun?
                jz      aux_noerr       ; no error
                call    print_string
aux_noerr       pop     dx
                mov     ah, dh          ; restore AH
                pop     dx
                retf


;-----------------------------------------------------------------------------
; send character to serial
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
AUXOUT          push    ax
                push    dx
                mov     ah, 1
                mov     dx, 0
                int     0x14            ; send character to serial port
                test    ah, 0x80        ; timeout error?
                jz      pop_dx_ax_retf  ; no all fine
                mov     dx, ERR_AUX
                jmp     printer_error


;-----------------------------------------------------------------------------
; check for disk change
;  AH = flag (1=changed)
;-----------------------------------------------------------------------------
DSKCHG          mov     ah, 0           ; the IBM PC can't detect disk change
                retf


temp_sector:
;-----------------------------------------------------------------------------
; entry point from boot sector
;  assumes DX = 0
;-----------------------------------------------------------------------------
INIT            cli
                mov     ax, cs
                mov     ds, ax
                mov     ss, ax
                mov     sp, temp_sector_end; set stack used during init
                sti
                xor     ah, ah
                int     0x13            ; reset disk 0 (DX = 0)
                mov     al, 0xA3        ; 2400 8N1
                int     0x14            ; initialize serial port
                mov     ah, 1
                int     0x17            ; initialize printer
                int     0x11            ; get system info
                and     ax, 0xC0        ; number of floppies in bits 6 and 7
                mov     cx, 5
                shr     ax, cl          ; (floppies-1) * 2
                add     ax, 2           ; floppies * 2
                and     ax, 6           ; will become 0 for 4 floppies
                jz      four_floppies   ; 4 floppies (num_floppies pre-assigned with 4)
                cmp     al, 2           ; one floppy?
                jnz     multi_floppy    ; no
                shl     ax, 1           ; pretend we have two, we'll emulate one
                mov     byte [single_floppy], 1
multi_floppy    mov     bx, floppy_list
                add     bx, ax          ; + floppies * 2
                mov     word [bx], 0    ; terminate list with 2 zero words
                mov     word [bx+2], 0
                nop                     ; XXX original assembler wasted a byte
                shr     ax, 1           ; =floppies
                mov     [num_floppies], al
four_floppies   push    ds
                mov     ax, 0
                mov     ds, ax          ; DS := 0x0000
                mov     ax, SEG_BIO     ; target segment for interrupt vectors
                mov     [0x6E], ax      ; set INT 1Bh segment
                mov     word [0x6C], int_1B; set INT 1Bh offset
                mov     word [0x00], int_00; set INT 00h offset
                mov     [0x02], ax      ; set INT 00h segment
                mov     bx, iret1       ; set INT 00h offset
                mov     [0x04], bx      ; set INT 01h offset (empty)
                mov     [0x06], ax      ; set INT 01h segment
                mov     [0x0C], bx      ; set INT 03h offset (empty)
                mov     [0x0E], ax      ; set INT 03h segment
                mov     [0x10], bx      ; set INT 04h offset (empty)
                mov     [0x12], ax      ; set INT 04h segment
                mov     ax, 0x50
                mov     ds, ax          ; DS := 0x0050
                mov     word [0x0], 0   ; clear 0x0500 in DOS Comm. Area (???)
                push    es
                mov     ax, SEG_DOS     ; target segment for IBMDOS.COM
                mov     es, ax
                mov     cx, DOS_SIZE/2  ; size/2 of IBMDOS.COM
                cld
                mov     ax, SEG_DOS_TEMP; source segment of IBMDOS.COM
                mov     ds, ax          ; the booloader read whole sectors and puts
                xor     di, di          ; the IBMDOS.COM image right after this;
                mov     si, di          ; so move it down a little
                rep movsw               ; copy 10 000 bytes from 0xE00 to 0xB10
                pop     es
                pop     ds
                mov     si, num_floppies; pass in pointer to structure
                call    SEG_DOS:0       ; init DOS (returns DS = memory for COMMAND.COM)
                sti
                mov     dx, 0x0100      ; 0x0100 in COMMAND.COM segment
                mov     ah, 0x1A
                int     0x21            ; set disk transfer area address
                mov     cx, [0x06]      ; remaining memory size
                sub     cx, 0x0100      ; - Program Segment Prefix = bytes to read
                mov     bx, ds
                mov     ax, cs
                mov     ds, ax
                mov     dx, FCB_command_com; File Control Block
                mov     ah, 0x0F
                int     0x21            ; DOS: open COMMAND.COM
                or      al, al
                jnz     error_command   ; error opening COMMAND.COM
                mov     word [FCB_command_com+0x21], 0; random record field
                mov     word [FCB_command_com+0x23], 0;  := 0x00000000
                mov     word [FCB_command_com+0x0E], 1; record length = 1 byte
                mov     ah, 0x27
                int     0x21            ; DOS: read
                jcxz    error_command   ; read 0 bytes -> error
                cmp     al, 1
                jnz     error_command   ; end of file not reached -> error
                mov     ds, bx
                mov     es, bx          ; DS := ES := SS := COMMAND.COM
                mov     ss, bx
                mov     sp, 0x40        ; 64 byte stack in PSP (XXX interrupts are on!)
                xor     ax, ax
                push    ax              ; push return address 0x0000 (int 0x20)
                mov     dx, [0x80]      ; get new DTA address
                mov     ah, 0x1A
                int     0x21            ; set disk transfer area address
                push    bx              ; segment of COMMAND.COM
                mov     ax, 0x0100      ; offset of COMMAND.COM entry
                push    ax
                retf                    ; run COMMAND.COM


error_command:  mov     dx, ERR_COMMANDCOM ; "rnBad or missing Command Interprete"
                call    print_string
halt            jp      halt    ; XXX jp instead of jmp


;-----------------------------------------------------------------------------


FCB_command_com db 1, 'COMMAND CO','M'+0x80
                times 19h db 0


;-----------------------------------------------------------------------------


ERR_COMMANDCOM  db 13,10,'Bad or missing Command Interprete','r'+0x80,13,10,0


;-----------------------------------------------------------------------------


; this is passed to IBMDOS.COM
num_floppies    db 4                    ; if there's 1 physical drive, this says 2
floppy_list     dw parameters           ; point to params for every floppy installed; 0-terminated
                dw parameters
                dw parameters
                dw parameters
                dw 0,0


parameters      dw SECTOR_SIZE
                db 1                    ; will be decremented by 1, then used
                dw 1
                db 2
                dw 0x0040
                dw 320                  ; number of total sectors


;-----------------------------------------------------------------------------


                times 512-($-temp_sector) db 0
temp_sector_end:


;-----------------------------------------------------------------------------


printer_retry   db 0                    ; count for printer retries
next_char       db 0                    ; extra character in keyboard queue
                db 0                    ; XXX unused
single_floppy   db 0                    ; true if we emulate a second logical floppy


;-----------------------------------------------------------------------------
; READ  - read sector(s) from disk
; WRITE - write sector(s) to disk
;  al     drive number (0-3)
;  ds:bx  buffer
;  cx     count
;  dx     logical block number
;-----------------------------------------------------------------------------
READ            mov     ah, 2           ; BIOS code "read"
                jmp     short read_write
WRITE           mov     ah, 3           ; BIOS code "write"
read_write      push    es
                push    ds
                push    ds
                pop     es              ; ES := DS
                push    cs
                pop     ds              ; DS := CS
                mov     [temp_sp], sp   ; save sp for function abort
                mov     [int_13_cmd], ah; save whether it was read or write
; logic to emulate a "logical" drive B: by prompting the user to change disk
; when the currently used drive is changed
                cmp     byte [single_floppy], 1
                jnz     multi_drive     ; more than one drive
                push    ds
                xor     si, si
                mov     ds, si          ; DS := 0x0000
                mov     ah, al
                xchg    ah, [LOGICAL_DRIVE]; current logical drive
                pop     ds
                cmp     al, ah
                jz      drive_unchanged
                push    dx              ; save block number
                add     al, 'A'
                mov     [TXT_DRIVE], al
                mov     dx, TXT_INSERTDISK
                call    print_string    ; prompt for disk change
                push    ds
                xor     bp, bp
                mov     ds, bp
                mov     byte [KEYBUF_NEXT], KEYBUF & 0xFF
                mov     byte [KEYBUF_FREE], KEYBUF & 0xFF; clear keyboard buffer
                pop     ds
                mov     ah, 0
                int     0x16            ; wait for any key
                pop     dx              ; block number
drive_unchanged mov     al, 0           ; for both logical A: or B: use drive A:
multi_drive     xchg    ax, dx
                mov     dh, 8           ; convert LBA to CHS
                div     dh              ; al = track (starts at 0)
                inc     ah              ; ah = sector (starts at 1)
                xchg    al, ah          ; track and sector
                xchg    ax, cx          ; cx = t/s, ax = count
                mov     [num_sectors], ax; count
                mov     dh, 0
; work around DMA hardware bug in case I/O spans a 64 KB boundary
; by using a temporary buffer
                mov     di, es          ; destination segment
                shl     di, 1
                shl     di, 1           ; make es:bx a linear address
                shl     di, 1           ; (discard upper bits)
                shl     di, 1
                add     di, bx
                add     di, SECTOR_SIZE-1; last byte of sector (linear)
                jc      across_64k      ; sector overflows it
                xchg    bx, di          ; bx = last byte, di = buffer
                shr     bh, 1           ; sector index in memory
                mov     ah, 0x80        ; 0x80 sectors fit into 64 KB
                sub     ah, bh          ; sectors until 64 KB boundary
                mov     bx, di          ; bx = buffer
                cmp     ah, al          ; compare to number of sectors
                jbe     skip2           ; they fit into 64 KB, cap num
                mov     ah, al          ; don't cap number of sectors
skip2           push    ax
                mov     al, ah          ; al = count
                call    rw_tracks
                pop     ax
                sub     al, ah          ; requested = done?
                jz      rw_done         ; yes, exit
across_64k      dec     al              ; one sector less
                push    ax
                cld
                push    bx
                push    es              ; save data pointer
                cmp     byte [int_13_cmd], 2
                jz      across_64k_read ; write case follows
                mov     si, bx
                push    cx
                mov     cx, SECTOR_SIZE/2; copy first sector
                push    es
                pop     ds
                push    cs
                pop     es
                mov     di, temp_sector
                mov     bx, di
                rep movsw               ; copy into IBMBIO local data
                pop     cx
                push    cs
                pop     ds
                call    rw_one_sector   ; write last sector
                pop     es
                pop     bx
                jmp     short across_64k_end
across_64k_read mov     bx, temp_sector
                push    cs
                pop     es
                call    rw_one_sector   ; read last sector into temp buffer
                mov     si, bx
                pop     es
                pop     bx
                mov     di, bx
                push    cx
                mov     cx, SECTOR_SIZE/2
                rep movsw               ; copy out
                pop     cx
across_64k_end  add     bh, 2           ; continue 0x0200 after that
                pop     ax
                call    rw_tracks
rw_done         pop     ds
                pop     es
                clc                     ; success
                retf


;-----------------------------------------------------------------------------
; read/write an arbirtary number of sectors
;-----------------------------------------------------------------------------
rw_tracks       or      al, al
                jz      ret2            ; nothing to read
                mov     ah, 9
                sub     ah, cl
                cmp     ah, al          ; more sectors than left in track?
                jbe     skip3           ; no
                mov     ah, al          ; otherwise, read up to end of track
skip3           push    ax
                mov     al, ah
                call    near rw_sectors ; reads/writes up to 8 sectors
                pop     ax
                sub     al, ah          ; decrease sectors to read
                shl     ah, 1
                add     bh, ah          ; advance pointer by sectors * 0x0200
                jmp     rw_tracks       ; continue


;-----------------------------------------------------------------------------


int_13_err      xchg    ax, di
                mov     ah, 0
                int     0x13            ; disk reset
                dec     si
                jz      translate       ; retries exhausted
                mov     ax, di
                cmp     ah, 0x80        ; in the "timeout (not ready)" case,
                jz      translate       ; we don't retry (this would take forever)
                pop     ax
                jmp     short retry
translate       push    cs
                pop     es
                mov     ax, di
                mov     al, ah          ; status
                mov     cx, 0x0A
                mov     di, conv_status
                repne scasb
                mov     al, [di+9]
                nop                     ; XXX original assembler wasted a byte
                mov     cx, [num_sectors]
                mov     sp, [temp_sp]   ; clean up stack
                pop     ds
                pop     es
                stc                     ; error
                retf


rw_one_sector   mov     al, 1


; reads/writes one or more sectors that are on the same track
rw_sectors      mov     si, 5           ; number of retries
                mov     ah, [int_13_cmd]
retry           push    ax
                int     0x13            ; perform the read/write
                jc      int_13_err
                pop     ax
                sub     [num_sectors], al
                add     cl, al          ; calculate next sector number
                cmp     cl, 8           ; exceeds track?
                jbe     ret2            ; no
                inc     ch              ; next track
                mov     cl, 1           ; sector 1
ret2            retn


;-----------------------------------------------------------------------------


TXT_INSERTDISK  db 13,10,'Insert diskette for drive',' '+0x80
TXT_DRIVE       db 'A: and strik','e'+0x80,13,10
                db 'any key when read','y'+0x80,13,10,10,0


;-----------------------------------------------------------------------------


conv_status     db 0x80,0x40,0x20,0x10,9,8,4,3,2; BIOS error codes
                db 1,2,6,0x0C,4,0x0C,4,8,0,0x0C,0x0C; IBMBIO error codes


;-----------------------------------------------------------------------------


int_13_cmd      db 2   Â 
temp_sp         dw 0   Â 
num_sectors     db 0   Â 


;-----------------------------------------------------------------------------


                times 513 db 0
                db 0xC9
                times 126 db 0


;-----------------------------------------------------------------------------
