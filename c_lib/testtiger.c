#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>
#include <sys/resource.h>

#ifdef i386
#define ITERATIONS 30
#else
/* #define ITERATIONS 500 */
#define ITERATIONS 50000
#endif


#include "tiger.h"
#include "tigertree.h"

double cputime()
{
    struct rusage usage;
    if(getrusage( RUSAGE_SELF, &usage ) != 0) return 0;
    return ((double)usage.ru_utime.tv_sec + usage.ru_utime.tv_usec*1.0e-6);
    /*
    return (((double)clock())/((double)CLOCKS_PER_SEC));
    */
}

void printrate(double rate)
{
    static const char *suffixes[] = {"bit", "Kbit", "Mbit", "Gbit", "Tbit", NULL};
    int i;

    for (i=0; (suffixes[i] != NULL) && (rate >= 512); ++i, rate /= 1024);
    printf("rate = %lf %s/s\n", rate, suffixes[i]);
}


int main()
{
  uint8_t buffer[65536];
  double t1, t2;
  double rate;
  int i;

  uint64_t res[3];
  tiger_context *ctx;
  tigertree_context *tctx;

#define hash(str) ctx = tiger_new(); tiger_feed(ctx, str, strlen(str)); tiger_finalize(ctx, res); tiger_free(ctx); \
  printf("Hash of \"%s\":\n\t%08X%08X %08X%08X %08X%08X\n", \
	 str, \
	 ntohl((uint32_t)(res[0])), \
	 ntohl((uint32_t)(res[0]>>32)), \
	 ntohl((uint32_t)(res[1])), \
	 ntohl((uint32_t)(res[1]>>32)), \
	 ntohl((uint32_t)(res[2])), \
	 ntohl((uint32_t)(res[2]>>32)) );

  /* Hash of short strings */
  hash("");
  hash("abc");
  hash("Tiger");
  /* Hash of 512-bit strings */
  hash("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-");
  hash("ABCDEFGHIJKLMNOPQRSTUVWXYZ=abcdefghijklmnopqrstuvwxyz+0123456789");
  hash("Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham");
  /* Hash of two-block strings strings */
  hash("Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham, proceedings of Fast Software Encryption 3, Cambridge.");
  hash("Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham, proceedings of Fast Software Encryption 3, Cambridge, 1996.");
  hash("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-");
  /* Hash of a 64K byte string */
  for (i=0;i<sizeof(buffer);i++)
    buffer[i] = i&0xFF;

  ctx = tiger_new();
  tiger_feed(ctx, buffer, sizeof(buffer));
  tiger_finalize(ctx, res);
  tiger_free(ctx);
  printf("Hash of a 64K-byte string:\n\t%08X%08X %08X%08X %08X%08X\n",
	 (uint32_t)(res[0]>>32),
	 (uint32_t)(res[0]),
	 (uint32_t)(res[1]>>32),
	 (uint32_t)(res[1]),
	 (uint32_t)(res[2]>>32),
	 (uint32_t)(res[2]) );
  //t1 = clock();
  t1 = cputime();
  ctx = tiger_new();
  for (i=0;i<ITERATIONS;i++)
    {
      //tiger_init(ctx);
      //tiger_reset(ctx);
      tiger_feed(ctx, buffer, sizeof(buffer));
      //tiger_finalize(ctx, res);
      //tiger_done(ctx);
    }
  tiger_finalize(ctx, res);
  tiger_free(ctx);
  //t2 = clock();
  t2 = cputime();

  /*
  rate = (double)CLOCKS_PER_SEC*(double)ITERATIONS*65556.0*8.0/
         ((double)(t2 - t1));
  */


  rate = ((double)ITERATIONS)*sizeof(buffer)*8.0/(t2-t1);
  /* printf("rate = %lf bit/s\n", rate); */

  printrate(rate);

  t1 = cputime();
  tctx = tigertree_new();
  for (i=0;i<ITERATIONS;i++)
    {
      //tigertree_reset(tctx);
      tigertree_feed(tctx, buffer, sizeof(buffer));
      //tigertree_finalize(tctx, res);
    }
  tigertree_finalize(tctx, res);
  tigertree_free(tctx);
  t2 = cputime();

  /*
  rate = (double)CLOCKS_PER_SEC*(double)ITERATIONS*65556.0*8.0/
         ((double)(t2 - t1));
  */


  rate = ((double)ITERATIONS)*sizeof(buffer)*8.0/(t2-t1);
  /* printf("rate = %lf bit/s\n", rate); */
  printrate(rate);

  return 0;
}
