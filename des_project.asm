; multi-segment executable file template.

data segment
    ;left side of the the message after converted to hexa
    L_hexa_message db 01h,23h,45h,67h
    ;right side of the the message after converted to hexa
    R_hexa_message db 89h,0ABh,0CDh,0EFh 
    ;encryption key that was choosed by the user
    ekey db 13h,34h,57h,79h,9Bh,0BCh,0DFh,0F1h
    ;pc-1 means - bit arrangment to create the permuted key - k+
    pc_1 db 57,49,41,33,25,17,09
         db 01,58,50,42,34,26,18
         db 10,02,59,51,43,35,27
         db 19,11,03,60,52,44,36
         db 63,55,47,39,31,23,15
         db 07,62,54,46,38,30,22
         db 14,06,61,53,45,37,29
         db 21,13,05,28,20,12,04
    
    k_plus db 7 dup(00)
    selected_byte db ?
    selected_bit db  ?
    temp_byte db 00000000b
    byte_builder_pointer db 0
    pointer db offset pc_1
    k_plus_l dd 0h
    k_plus_r dd 0h
         
    
    
    
    
ends

stack segment
    dw   128  dup(0)
ends

code segment
;the proc rearrange the ekey according to the permution table pc-1, and then puts it in k_plus
;use selected_byte to know the specific byte in the ekey, use selected bit to know the specific bit it the byte                                          
;may use temp_byte as helper
proc permute_key_pc_1
     
     pusha
     xor ax,ax
     xor bx,bx
     xor cx,cx
     xor dx,dx
     mov temp_byte,00000000b
     mov byte_builder_pointer,0h
     ;mov pointer,offset pc_1

     mov cx,8
     again:
     xor bx,bx
     xor ax,ax
     mov bl,pointer
     mov al,[bx]
     xor bx,bx
     mov dx,8
     div dl
     mov selected_byte,al;**** ;maybe al and ah need to be switched, you need to check it out
     mov selected_bit,ah
     mov bx,offset ekey
     add bl,selected_byte ;bx gets the offset of the selected byte
     mov dl,[bx] ;the selected byte copy to dl
     
     ;finds the selected bit using "and" and "shl"
     ;you need to add "cmp" before the "jnz endd".you need to add shr's(after delete this comment)
     cmp selected_bit,1
     jnz skip_1
        and dl,10000000b
     jmp endd
     skip_1:
     cmp selected_bit,2
     jnz skip_2
        and dl,01000000b
        shl dl,1
     jmp endd
     skip_2:
     cmp selected_bit,3
     jnz skip_3
        and dl,00100000b
        shl dl,2
     jmp endd
     skip_3:
     cmp selected_bit,4
     jnz skip_4
        and dl,00010000b
        shl dl,3
     jmp endd
     skip_4:
     cmp selected_bit,5
     jnz skip_5
        and dl,00001000b
        shl dl,4
     jmp endd
     skip_5:
     cmp selected_bit,6
     jnz skip_6
        and dl,00000100b
        shl dl,5
     jmp endd
     skip_6:
     cmp selected_bit,7
     jnz skip_7
        and dl,00000010b
        shl dl,6
     jmp endd
     skip_7:
     cmp selected_bit,8
     jnz endd
        and dl,00000001b
        shl dl,7
     endd:
        nop
        
        
     cmp byte_builder_pointer,0
     
     jnz bit_1 
        
        cmp dl,10000000b
        jnz end_bit_0
        xor temp_byte,10000000b
        end_bit_0:   
        jmp end_builder
        
     bit_1:
        
        cmp byte_builder_pointer,1
        
        jnz bit_2
        
        cmp dl,10000000b
        jnz end_bit_1
        xor temp_byte,01000000b
        end_bit_1:
        jmp end_builder
        
     bit_2:
    
       cmp byte_builder_pointer,2
       
       jnz bit_3
       
        cmp dl,10000000b
        jnz end_bit_2
        xor temp_byte,00100000b
        end_bit_2:
        jmp end_builder
        
     bit_3:
    
       cmp byte_builder_pointer,3
       
       jnz bit_4
       
        cmp dl,10000000b
        jnz end_bit_3
        xor temp_byte,00010000b
        end_bit_3:
        jmp end_builder
     
     bit_4:
     
       cmp byte_builder_pointer,4
       
       jnz bit_5
       
        cmp dl,10000000b
        jnz end_bit_4
        xor temp_byte,00001000b
        end_bit_4:
        jmp end_builder
        
     bit_5:
     
       cmp byte_builder_pointer,5
       
       jnz bit_6
       
        cmp dl,10000000b
        jnz end_bit_5
        xor temp_byte,00000100b
        end_bit_5:
        jmp end_builder
        
     bit_6:
     
       cmp byte_builder_pointer,6
       
       jnz bit_7
       
        cmp dl,10000000b
        jnz end_bit_6
        xor temp_byte,00000010b
        end_bit_6:
        jmp end_builder
        
     bit_7:
     
       cmp byte_builder_pointer,7
       
       jnz end_builder
       
        cmp dl,10000000b
        jnz end_bit_7
        xor temp_byte,00000001b
        end_bit_7:
        end_builder:
            nop
        
        
        inc pointer
        inc byte_builder_pointer   
        loop again
        
            popa
            ret
endp permute_key_pc_1 

proc arrange_k_plus
     
     pusha
     xor ax,ax
     ;1st byte
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus,al 
     ;2nd byte
     mov pointer, offset pc_1+8
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+1,al
     ;3rd byte
     mov pointer, offset pc_1+16
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+2,al       
     ;4th byte
     mov pointer, offset pc_1+24
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+3,al
     ;5th byte
     mov pointer, offset pc_1+32
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+4,al     
     ;6th byte
     mov pointer, offset pc_1+40
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+5,al                    
     ;7th byte
     mov pointer, offset pc_1+48
     call permute_key_pc_1
     mov al,temp_byte
     mov k_plus+6,al                    
     
     
     xor bx,bx
     mov bl,k_plus+1
     mov k_plus_l+2,bx
     mov bl,k_plus
     mov k_plus_l+3,bx
     
     mov bl,k_plus+3
     mov bh,k_plus+2
     and bl,11110000b
     mov k_plus_l,bx
     
     xor bx,bx
     mov bh,k_plus+3
     and bh,00001111b
     mov bl,k_plus+4
     mov k_plus_r+2,bx
     mov bh,k_plus+5
     mov bl,k_plus+6
     mov k_plus_r,bx
     
        
     popa
     ret
endp arrange_k_plus   
     
        
      
     
     
     
     
         
    
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    call arrange_k_plus
    
    

end start ; set entry point and stop the assembler.
