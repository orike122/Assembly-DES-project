; multi-segment executable file template.

data segment
    ; add your data here!
    msg db 8 dup(0)
    temp_byte db 0
    msg_ascii db 19 dup(0)
    error_msg db "You typed in some illigeal charecters, please insert hexadecimal number up to 16 charecter, not h nor zero before letters alowed. Press any key to try again...","$"
    ask_msg db "Please enter the 16 charecters length hexa message: ","$"
    ask_key db "Please enter the 16 charecters length hexa encryption key: ","$"
    key_ascii db 19 dup(0)
    key db 8 dup(0)
    char_pointer db 0
ends


stack segment
    dw   128  dup(0)
ends

code segment

macro get_input dest,len
     pusha
     mov dx,offset dest
     mov bx,dx
     mov cl,len
     inc cl
     mov [bx],cl
     mov ah,0ah
     int 21h
     popa
endm get_input

macro str2hex source,dest
     pusha 
     LOCAL agn_bld_char,goback,error,check_hex,skp_hex,check_cap,skp_1st_char,end_str
     xor di,di
     xor si,si
     mov si,2
     xor cx,cx
     mov cx,8
     agn_bld_char:
     mov char_pointer,0
     xor ax,ax
     xor dx,dx
     xor bx,bx
     goback:
     mov al,source+si

     ;check
     cmp al,30h
     js error
     mov bl,39h
     cmp bl,al
     js check_hex
     sub al,30h
     jmp skp_hex
     
     check_hex:
     cmp al,41h
     js error
     mov bl,46h
     cmp bl,al
     js check_cap
     sub al,55
     jmp skp_hex
     
     check_cap:
     cmp al,61h
     js error
     mov bl,66h
     cmp bl,al
     js error
     sub al,87
     
     skp_hex:
     
     cmp char_pointer,0
     jnz skp_1st_char
     inc si
     inc char_pointer
     mov dh,al
     jmp goback
     skp_1st_char:
     mov bl,10h
     mov dl,al
     mov al,dh
     mul bl
     mov dh,al
     or dh,dl
     mov dest+di,dh
     inc si
     inc di
     loop agn_bld_char
     
     jmp end_str
     error:
     print_msg error_msg
     ; wait for any key....    
     mov ah, 1
     int 21h
     call get_msgnkey

     end_str:
     popa
endm str2hex

macro print_msg pmsg
      pusha
      call cln_scr
      set_cur 0,0
      mov ah,09h
      mov dx,offset pmsg
      int 21h


      popa
endm print_msg

proc cln_scr
     pusha
      ;clean screen
      mov ah,6
      mov al,0
      mov bh,0_0fh
      xor cx,cx
      mov dh,200
      mov dl,200
      int 10h
      
     popa
     ret
endp cln_scr

macro set_cur x,y
      ;set cursor
      pusha
      mov ah,2
      mov bh,0
      xor dx,dx
      int 10h
      popa
endm set_cur x,y

proc get_msgnkey
     pusha
     print_msg ask_msg
     get_input msg_ascii,16
     str2hex msg_ascii,msg
     print_msg ask_key
     get_input key_ascii,16
     str2hex key_ascii,key
     popa
     ret
endp get_msgnkey
     
     
     

     
      
      
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    call get_msgnkey            
    mov ax, 4c00h ; exit to operating system.
    int 21h  
    
ends

end start ; set entry point and stop the assembler.
