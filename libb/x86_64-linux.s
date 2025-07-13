.text
.globl char
.globl lchar

char:
xorq %rax, %rax
movb (%rdi, %rsi), %al
ret

lchar:
movb %dl, (%rdi, %rsi)
ret
