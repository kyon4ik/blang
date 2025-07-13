.text
.globl char
.globl lchar

char:
ldrb w0, [x0, x1]
ret

lchar:
strb w2, [x0, x1]
ret
