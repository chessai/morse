#include <stdio.h>
#include <string.h>
char buf[99], tree[] = " ETIANMSURWDKGOHVF L PJBXCYZQ  ";
int main() {
    while (scanf("%s", buf)) {
        int n, t=0;
        for(n=0; buf[n]!=0; n++)
            t = 2*t + 1 + (buf[n]&1);   /* compute offset */
        putchar(tree[t]);               /* fetch letter   */
    }
}
