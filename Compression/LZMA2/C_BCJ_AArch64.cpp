extern "C" {
#include "C_BCJ_AArch64.h"
}

#include "C/BraAArch64.c"

int bcj_arm64_de_compress (int encoding, CALLBACK_FUNC *callback, void *auxdata)
{
  UInt32 ip = 0;
  BYTE* Buf = (BYTE*) malloc(LARGE_BUFFER_SIZE);
  if (Buf==NULL)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  int RemainderSize=0;
  int x, InSize;
  while ( (InSize = x = callback ("read", Buf+RemainderSize, LARGE_BUFFER_SIZE-RemainderSize, auxdata)) >= 0 )
  {
    if ((InSize+=RemainderSize)==0)    goto Ok;
    int OutSize = InSize<=4? InSize : (int)AArch64_Convert(Buf, InSize, ip, encoding);
    ip += OutSize;
    if( (x=callback("write",Buf,OutSize,auxdata)) != OutSize )      goto Error;
    RemainderSize = InSize-OutSize;
    if (RemainderSize>0)                memmove(Buf,Buf+OutSize,RemainderSize);
  }
Error: free(Buf); return x;
Ok:    free(Buf); return FREEARC_OK;
}


/*-------------------------------------------------*/
/* BCJ_ARM64_METHOD methods                       */
/*-------------------------------------------------*/
int BCJ_ARM64_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_arm64_de_compress (0, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int BCJ_ARM64_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_arm64_de_compress (1, callback, auxdata);
}

void BCJ_ARM64_METHOD::ShowCompressionMethod (char *buf)
{
  sprintf (buf, "arm64");
}

#endif  /* !defined (FREEARC_DECOMPRESS_ONLY) */

COMPRESSION_METHOD* parse_BCJ_ARM64 (char** parameters)
{
  if (strcmp (parameters[0], "arm64") == 0
      &&  parameters[1]==NULL )
    return new BCJ_ARM64_METHOD;
  else
    return NULL;
}

static int BCJ_ARM64_x = AddCompressionMethod (parse_BCJ_ARM64);
