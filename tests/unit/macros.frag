// Define a bunch of macros. Make sure Shader Minifier doesn't use
// these names when renaming variables.
// Also, test that the spaces are stripped.
#define a $
#define b (    $+)
#define c   $  +
#define d abc    def  + ghi ()
#define e $
#define f $
#define g $
#define h $
#define i $
#define j $
#define k $
#define l $
#define m $
#define n $
#define o $
#define p $

int foo() {
  int var1 = 1, var2 = 2, var3 = 3, var4 = 4, var5 = 5, var6 = 6;
  int var7 = 7, var8 = 8, var9 = 9, var10 = 10, var11 = 11, var12 = 12;
  return var1 + var5 + var12;
}
