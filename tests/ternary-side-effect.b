foo() {
    extrn printf;
    printf("  Foo*n");
}

bar() {
    extrn printf;
    printf("  Bar*n");
}

main() {
    extrn printf, foo, bar;
    printf("Only Foo should be printed bellow:*n");
    1 ? foo() : bar();
}
