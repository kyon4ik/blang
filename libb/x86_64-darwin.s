.text
.globl _char
.globl _lchar

_char:
xorq %rax, %rax
movb (%rdi, %rsi), %al
ret

_lchar:
movb %dl, (%rdi, %rsi)
ret
