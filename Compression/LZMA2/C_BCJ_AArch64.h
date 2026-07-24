#include "../Compression.h"
int bcj_arm64_de_compress (int encoding, CALLBACK_FUNC *callback, void *auxdata);

#ifdef __cplusplus

class BCJ_ARM64_METHOD : public COMPRESSION_METHOD
{
public:
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);
  virtual void ShowCompressionMethod (char *buf);
  virtual MemSize GetCompressionMem     (void)         {return LARGE_BUFFER_SIZE;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {}
  virtual void    SetDecompressionMem   (MemSize mem)  {}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
  virtual MemSize GetDecompressionMem   (void)         {return LARGE_BUFFER_SIZE;}
};

COMPRESSION_METHOD* parse_BCJ_ARM64 (char** parameters);

#endif  /* __cplusplus */
