/*
 * from the traceback package, written by Clem Dickey
 * (C) Copyright IBM Corp. 1991
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <sys/debug.h>
 
#ifndef FRAME_LAYOUT
#define FRAME_LAYOUT
 
typedef void code;
 
struct frame;
 
struct links
{
	struct frame *sp;
	long cr;
	code *lr;
	code *buff[2];
	int *toc;
};
 
struct frame
{
	struct links linkarea;
	int parms[8];
};
 
struct tocentry
{
	code *module;
	int *toc;
	void *pdsa; /* environment pointer "for languages such as Pascal and PL/I" */
};
 
struct frame *fpThisFrame( int );
struct frame *fpPrevFrame( struct frame * );
int cwSizeOfFrame( struct frame * );
unsigned cwCopyFrame( int *, struct frame *, struct frame * );
 
#endif

/*
 * Given a frame (which includes a Link Register), return the traceback table
 * for that frame.
 */
 
static struct tbtable *tbpFindTracebackTable( struct frame *pFrame )
{
	int *wp = (int *)pFrame->linkarea.lr;
	while(*wp++);
	return (struct tbtable *)wp;
}
 
/*
 * return the caller's frame pointer.  It is 6 words below the address of
 * our parameter, in rios stack format.
 */
 
struct frame *fpThisFrame(int i)
{
	return (struct frame *)( (char *)&i - sizeof(struct links) );
}
 
struct frame *fpPrevFrame( struct frame *fpCurrent )
{
	return fpCurrent->linkarea.sp;
}
 
int cwSizeOfFrame( struct frame *fp )
{
	return (int *)fpPrevFrame( fp ) - (int *)fp;
}

/*
 * Code to retrieve a function's name on AIX.
 * When using xlC, compile with -Q! option (no inlining).
 */

mpitrace_caller(int depth, char * procname)
{
        int level;
        size_t length;
	struct frame *pFrame = fpPrevFrame( fpThisFrame(0) );
	struct tbtable *tbpTrace;
	struct tbtable_ext *x = 0;

        /* initialize a pointer to a traceback table */
	tbpTrace = tbpFindTracebackTable( pFrame );

        level = 0;

        /* loop through the call chain */
        while (tbpTrace->tb.version==0 && tbpTrace->tb.name_present)
        {
		char *c = (char *)&tbpTrace->tb_ext;
                /* discard missing fields */
		if (tbpTrace->tb.fixedparms+tbpTrace->tb.floatparms==0)
			c -= sizeof(x->parminfo);
		if (tbpTrace->tb.has_tboff==0) c -= sizeof(x->tb_offset);
		if (tbpTrace->tb.int_hndl==0) c -= sizeof(x->hand_mask);
		if (tbpTrace->tb.has_ctl)
		{
                   x = (struct tbtable_ext *) c;
                   c += x->ctl_info*sizeof(x->ctl_info_disp);
		} 
                else c -= sizeof(x->ctl_info);
		c -= sizeof(x->ctl_info_disp);
		x = (struct tbtable_ext *) c;
                length = x->name_len;
                if (level == depth)
                {
                   memcpy((void *)procname, &(x->name), length);
                   procname[length] = '\0';
                   return;
                }
                else
                {
                   pFrame = fpPrevFrame( pFrame );
                   tbpTrace = tbpFindTracebackTable( pFrame );
                   level ++;
                }
	}

       *procname = '\0';
        return;
}
