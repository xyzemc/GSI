/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: get_zeits - a C interface to times for Fortran calls
!
! !DESCRIPTION:
!
! !INTERFACE: */
 /*
  System times() dependencies:
 */


/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53J src/bos/kernel/sys/types.h 1.52.1.95                            */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1985,2006          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)55       1.52.1.95  src/bos/kernel/sys/types.h, incstd, bos53J, j2006_21B0 5/15/06 07:19:40 */
/*
 *   COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 *   FUNCTIONS:
 *
 *   ORIGINS: 3, 26, 27
 *
 *
 *   (C) COPYRIGHT International Business Machines Corp. 1985, 1996
 *   All Rights Reserved
 *   Licensed Materials - Property of IBM
 *   US Government Users Restricted Rights - Use, duplication or
 *   disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 */
/*
 *   Copyright (c) 1982, 1986 Regents of the University of California.
 *   All rights reserved.  The Berkeley software License Agreement
 *   specifies the terms and conditions for redistribution.
 *
 *      (#)types.h     7.1 (Berkeley) 6/4/86
 */




/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53N src/bos/usr/include/standards.h 1.10.2.4                        */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* COPYRIGHT International Business Machines Corp. 1995,2007              */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)44  1.10.2.4  src/bos/usr/include/standards.h, incstd, bos53N, n2007_38A8 9/13/07 12:37:30 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1995,2006
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */


/* These directives must be processed in the current order when compiled with 
 * cc or they will not work correctly.
 */

/* If _XOPEN_SOURCE is defined without a value, or with a value less
 * than 500 (UNIX98), then set a value, so that #if statements will 
 * work properly.
 */












/* 
 * Handle the use of the restrict keyword in non-C99 compilers
 */

/* 
 * Determine when C99 interfaces and definitions are allowed to be exposed
 *  - if _POSIX_C_SOURCE is newer than 1995 
 *  - if a C99 compiler is being used outside of the UNIX98 namespace
 */

/*
 * Determine what type should be used for the boolean type.
 * In C++ the type bool should be used.  If C99 is supported
 * then use _Bool.  Otherwise use unsigned char.
 */



/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/strict_stdtypes.h 1.1                       */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)28	1.1  src/bos/usr/include/strict_stdtypes.h, incstd, bos530 2/19/03 10:23:14 */
/* 
 * Restrict content from standard headers to the allowable typedefs.
 * This affects sys/types.h, inttypes.h, and stdint.h and any headers 
 * which include those headers.
 *
 * This file should be used in conjunction with <end_strict_stdtypes.h>, which
 * terminates the restricted environment.
 *
 * This has no effect if _ALL_SOURCE is defined.
 */



/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/kernel/sys/inttypes.h 1.24                              */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1997,2003          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)42     1.24  src/bos/kernel/sys/inttypes.h, incstd, bos530 6/13/03 14:02:19 */

/*
 *   COMPONENT_NAME: incstd
 *
 *   FUNCTIONS: none
 *
 *   ORIGINS: 27
 *
 *
 *   (C) COPYRIGHT International Business Machines Corp. 1997
 *   All Rights Reserved
 *   Licensed Materials - Property of IBM
 *   US Government Users Restricted Rights - Use, duplication or
 *   disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */






/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53A src/bos/kernel/sys/stdint.h 1.10                                */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2002,2004          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)09     1.10  src/bos/kernel/sys/stdint.h, incstd, bos53A, a2004_31E1 7/21/04 18:53:26 */
/*
 *   COMPONENT_NAME: incstd
 *
 *   FUNCTIONS: none
 *
 *   ORIGINS: 27
 *
 */


/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53N src/bos/usr/include/standards.h 1.10.2.4                        */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* COPYRIGHT International Business Machines Corp. 1995,2007              */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)44  1.10.2.4  src/bos/usr/include/standards.h, incstd, bos53N, n2007_38A8 9/13/07 12:37:30 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1995,2006
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */


	
/*
 * Basic / Extended integer types
 *
 * The following defines the basic fixed-size integer types.
 *
 */
typedef signed char		int8_t;
typedef signed short		int16_t;
typedef signed int		int32_t;
typedef signed long		int64_t;

typedef unsigned char		uint8_t;
typedef unsigned short		uint16_t;
typedef unsigned int		uint32_t;
typedef unsigned long		uint64_t;

/*
 * intmax_t and uintmax_t are to be the longest (in number of bits) signed
 * and unsigned integer types supported by the implementation.
 */
typedef int64_t			intmax_t;
typedef uint64_t		uintmax_t;

/*
 * intptr_t and uintptr_t are signed and unsigned integer types large enough
 * to hold any data pointer; that is, data pointers can be assigned into or
 * from these integer types without losing precision.
 */
typedef signed long		intptr_t;
typedef unsigned long		uintptr_t;

/*
 * The following define the smallest integer types that can hold the
 * specified number of bits.
 */
typedef signed char		int_least8_t;
typedef signed short		int_least16_t;
typedef signed int		int_least32_t;
typedef signed long		int_least64_t;

typedef unsigned char		uint_least8_t;
typedef unsigned short		uint_least16_t;
typedef unsigned int		uint_least32_t;
typedef unsigned long		uint_least64_t;

/* New typedef's and define's for ISO-c99 standard not a part of c89 */

/* Most efficient types of N or more bytes */
typedef signed char	int_fast8_t;
typedef int16_t		int_fast16_t;
typedef int32_t		int_fast32_t;
typedef unsigned char	uint_fast8_t;
typedef uint16_t	uint_fast16_t;
typedef uint32_t	uint_fast32_t;
typedef int64_t		int_fast64_t;
typedef uint64_t	uint_fast64_t;

/* implementation limits */

















/* Macros for creating constants of the types defined above*/











typedef struct {
	intmax_t quot;
	intmax_t rem;
} imaxdiv_t;

extern intmax_t strtoimax(const char * restrict, char ** restrict, int);

extern intmax_t imaxabs(intmax_t);
extern imaxdiv_t imaxdiv (intmax_t, intmax_t); 
extern uintmax_t strtoumax(const char * restrict, char ** restrict, int);
/* wchar_t is needed for wcsto(iu)max */
typedef unsigned int    wchar_t;
extern intmax_t wcstoimax(const wchar_t * restrict, wchar_t ** restrict, int );
extern uintmax_t wcstoumax(const wchar_t * restrict, wchar_t ** restrict, int);

/* Most efficient types on platform */
typedef int64_t         intfast_t;
typedef uint64_t        uintfast_t;


/* printf Macros for format specifiers */




















/* scanf Macros for format specifiers */






















/* printf/scan macros for intmax, intfast, intptr */








/* These types provide fixed size types that preserve source compatibility
 * for 32 bit interfaces with long types in structures that shouldn't be
 * 64 bits wide in 64 bit compilation mode.
 */
typedef signed int	__long32_t;
typedef unsigned int	__ulong32_t;

/* These types provide variable size types that preserve source compatibility
 * for 32 bit interfaces with int types in structures that need to be
 * 64 bits wide in 64 bit compilation mode.
 */
typedef signed long	__long64_t;
typedef unsigned long	__ulong64_t;

/* These types provide variable size types that preserve source compatibility
 * for 32 and 64 bit application interfaces with int types in structures 
 * that need to be 64 bits wide in 64 bit kernel and/or kernel extensions.
 */
typedef signed int	int32long64_t;
typedef unsigned int	uint32long64_t;

/* These types provide variable size types that preserve source compatibility
 * for 32 and 64 bit application interfaces with long types in structures 
 * that need to be variant for 32 and 64 bit applications, but invariant
 * 32-bit for kernel and kernel extensions.
 */
typedef signed long	long32int64_t;
typedef unsigned long	ulong32int64_t;



/*
 * BSD fixed-size integer type additions to the above ISO-C types.
 *
 */
typedef signed char		int8;
typedef signed short		int16;
typedef signed int		int32;
typedef long			int64;

typedef unsigned char		u_int8;
typedef unsigned char		u_int8_t;
typedef unsigned short		u_int16;
typedef unsigned short		u_int16_t;
typedef unsigned int		u_int32;
typedef unsigned int		u_int32_t;
typedef unsigned long		u_int64;
typedef unsigned long		u_int64_t;





/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/end_strict_stdtypes.h 1.1                   */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)29	1.1  src/bos/usr/include/end_strict_stdtypes.h, incstd, bos530 2/19/03 10:23:29 */
/* 
 * This file should be used in conjunction with <strict_stdtypes.h>, which
 * defines a restricted content environment for standard headers.
 */

/* End restricted includes */


/*
 *
 *      The ANSI and POSIX standards require that certain values be in types.h.
 *      It also requires that if _ANSI_C_SOURCE or _POSIX_SOURCE is defined
 *	then ONLY those values are present. This header includes all the ANSI
 *	and POSIX required entries.
 *
 *      Other entries are included in _ALL_SOURCE.
 *
 */

/*
 * ANSI C required typedefs
 */

typedef long		ptrdiff_t;


typedef unsigned int	wctype_t;


typedef long	fpos_t;

typedef long long fpos64_t;


typedef long		time_t;

typedef int		clock_t;

typedef	unsigned long	size_t;


/*
 * shorthand type definitions for unsigned storage classes
 */
typedef	unsigned char	uchar_t;

typedef	unsigned short	ushort_t;
typedef	unsigned int	uint_t;
typedef unsigned long	ulong_t;

typedef	signed long	ssize_t;

/*
 * standard AIX type definitions
 */
typedef int		level_t;
typedef	__long64_t	daddr_t;	/* disk address */
typedef	int		daddr32_t;	/* size invariant 32-bit disk address */
typedef	int64_t		daddr64_t;	/* size invariant 64-bit disk address */
typedef	char *		caddr_t;	/* "core" (i.e. memory) address */
typedef	__ulong64_t	ino_t;		/* inode number (filesystem) */
typedef	uint_t 		ino32_t;	/* size invariant 32-bit inode number */
typedef	uint64_t 	ino64_t;	/* size invariant 64-bit inode number */
typedef short		cnt_t;
typedef	__ulong64_t	dev_t;		/* device number (major+minor) */
typedef uint_t 		dev32_t;	/* size invariant 32-bit devno */
typedef uint64_t 	dev64_t;	/* size invariant 64-bit devno */
typedef	int		chan_t;		/* channel number (minor's minor) */
typedef int		time32_t;		/* size invariant 32-bit time */
typedef int		pid32_t;		/* size invariant 32-bit pid */
typedef int		tid32_t;		/* size invariant 32-bit tid */
typedef uint64_t     pid64_t;     /* size invariant 64-bit pid  */
typedef uint64_t     tid64_t;     /* size invariant 64-bit tid  */
typedef int64_t      time64_t;    /* size invariant 64-bit time */

/* pointer types passed across system calls */
typedef unsigned int __ptr32;
typedef unsigned int __cptr32;

typedef int soff_t;			/* Unambiguous 32 bit file offset */

typedef long		off_t;		/* file offset (32/64) */

typedef long long 	off64_t;

typedef	long		paddr_t;

/* also defined in sys/ipc.h */
typedef	int32long64_t	key_t;

typedef __long64_t	timer_t;	/* timer id */
typedef int		timer32_t;	/* size invariant 32-bit timer id */
typedef int64_t 	timer64_t;	/* size invariant 64-bit timer id */
typedef	short		nlink_t;

/* also defined in sys/ipc.h */
typedef	uint_t		mode_t;		/* file mode */

/* uid_t and gid_t also defined in pwd.h, and sys/ipc.h */
typedef uint_t		uid_t;		/* user ID */
typedef uint_t		gid_t;		/* group ID */

typedef	__ptr32 	mid_t;		/* module ID	*/

typedef	int32long64_t	pid_t;		/* process ID */

typedef __long64_t	tid_t;		/* thread ID */
typedef char		slab_t[12];	/* security label */
typedef long		mtyp_t;		/* ipc message type */
typedef int             boolean_t;
typedef int     	crid_t;

typedef __long64_t	blkcnt_t; /* number of blocks in the file */
typedef __long64_t	blksize_t; /* used for block sizes */

typedef int		blkcnt32_t; /* size invariant 32-bit block count  */
typedef int		blksize32_t; /* size invariant 32-bit block size  */
typedef uint64_t	blkcnt64_t; /* size invariant 64-bit block count  */
typedef uint64_t	blksize64_t; /* size invariant 64-bit block size  */


typedef ulong_t 	fsblkcnt_t;	/* number of blocks in the FS */
typedef ulong_t		fsfilcnt_t;	/* number of files */

	typedef	int		wint_t;		/* Wide character */

typedef uint32long64_t	id_t;		/* General ID */
typedef unsigned int	useconds_t;	/* time in microseconds */
typedef signed   int	suseconds_t;	/* signed time in microseconds */
typedef long long       clockid_t;      /* clock ID type */

/* typedef and structure for signal mask */
/* This must correspond to the "struct sigset" structure in _ALL_SOURCE below */
typedef struct sigset_t	{
	unsigned long ss_set[4];
} sigset_t;

typedef struct {
	unsigned int losigs, hisigs;
} sigset32_t;

typedef struct {
	uint64_t ss_set[4];
} sigset64_t;

typedef int signal_t;

/* typedef for the File System Identifier (fsid).  This must correspond 
 * to the "struct fsid" structure in _ALL_SOURCE below.
 */
typedef struct fsid_t {
	unsigned int val[2];
} fsid_t;


typedef struct fsid64_t {
	uint64_t val[2];
} fsid64_t;

typedef void *pthread_attr_t;
typedef	void *pthread_condattr_t;	
typedef	void *pthread_mutexattr_t;

typedef	void *pthread_rwlockattr_t;

typedef	void *pthread_barrierattr_t;
	
typedef unsigned int pthread_t;
typedef unsigned int pthread_key_t;

typedef struct
{
	long	__mt_word[8];
}
pthread_mutex_t;

typedef struct
{
	long	__cv_word[6];
}
pthread_cond_t;

typedef struct
{
        long    __on_word[9];
}
pthread_once_t;

typedef struct
{
	long	__sp_word[3];
}
pthread_spinlock_t;

typedef struct
{
	long	__br_word[5];
}
pthread_barrier_t;

typedef struct
{
	long	__rw_word[10];
}
pthread_rwlock_t;



typedef struct  _quad { int val[2]; } quad;


/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/kernel/sys/POWER/m_types.h 1.21                         */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1985,1993          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)77       1.21  src/bos/kernel/sys/POWER/m_types.h, sysproc, bos530 1/18/01 12:00:21 */

/*
 *   COMPONENT_NAME: SYSPROC
 *
 *   FUNCTIONS: 
 *
 *   ORIGINS: 27
 *
 *
 *   (C) COPYRIGHT International Business Machines Corp. 1985,1993
 *   All Rights Reserved
 *   Licensed Materials - Property of IBM
 *   US Government Users Restricted Rights - Use, duplication or
 *   disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */




/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53H src/bos/kernel/sys/vm_types.h 1.35                              */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1999,2006          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)47	1.35  src/bos/kernel/sys/vm_types.h, sysvmm, bos53H, h2006_06C4 2/9/06 11:58:00 */





typedef int             vmid_t;         /* virtual memory object ID     */
typedef uint_t		vmhandle_t;	/* virtual memory handle        */
typedef int             vmid32_t;       /* 32bit size-invariant vmid_t  */
typedef uint_t		vmhandle32_t;	/* 32bit size invariant vmhandle_t*/


typedef long32int64_t	kvmid_t;	/* kernel's internal version */
typedef ulong32int64_t	kvmhandle_t;	/* kernel's internal version */

typedef long		vmid64_t;
typedef long		rpn64_t;
typedef long            cnt64_t;        /* 64-bit count */
typedef long            psize_t;        /* page size */

typedef int32long64_t	vmidx_t;	/* index for vm objects */
                                        /* VM_FULL_KEY() -      */
typedef uint32long64_t  vmfkey_t;       /* ..combined: hkey/ppkey/noex */
                                        /* VM_PROT_KEY() -      */
typedef uint32long64_t  vmprkey_t;      /* ..combined: hkey/ppkey/noex */
typedef int32long64_t	vmkey_t;	/* key for vm objects */
typedef int32long64_t	vmhwkey_t;      /* hardware KsKp key */
typedef int32long64_t	vpn_t;		/* virtual page number          */
typedef int32long64_t	rpn_t;		/* real page number             */

typedef unsigned long   ptex_t;         /* page table entry index */
typedef unsigned long   swhatx_t;       /* Software HAT index */

typedef uint32long64_t  esid_t;         /* use this for esids */

typedef ushort_t	aptx_t;		/* Index into apt, ahat */

typedef int             pdtx_t;         /* pdt index */
typedef short           psx_t;          /* page size index */
typedef ushort_t        pshift_t;       /* page size shift */
typedef ushort_t        sshift_t;       /* segment size shift */

typedef int             unidx_t;        /* unode index */
typedef int             snidx_t;        /* snode index */
typedef int             vmnodeidx_t;    /* generic node index (snode/unode) */

typedef int		kvpn_t;		/* kernel's internal view of vpn*/
   typedef int		krpn_t;		/* kernel's internal view of rpn*/
typedef int32long64_t   vmsize_t;	/* size param */

typedef int32long64_t   vmm_lock_t;     /* for vmmlock */


typedef unsigned long ureg_t;		/* unsigned register type */



typedef struct
vmaddr_t
{		/* long-form virtual address */
	vmhandle_t	srval;		/* segment reg contents */
	caddr_t		offset;		/* offset within segment */
} vmaddr_t;


typedef struct
adspace_t
{		/* address space mapping */
	ulong32int64_t	alloc;		/* allocation flags */
	vmhandle_t	srval[16];	/* contents of all seg regs */
} adspace_t;




/*
 * Memory Region Management Definitions and Structures
 */
typedef enum _MR_ATTR_TYPE {
  BadAttr = 0,
  VirtAddr = 1
} MR_ATTR_TYPE;

typedef enum _MR_LABEL_TYPE {
  BadMem = 0,
  FreeMem = 1,
  IPLCB = 2,
  RMALLOC = 3,
  PM_HEAP = 4,
  RTAS_HEAP = 5,
  TCE_TABLE = 6,
  IO_SPACE = 7,
  HUGE_PAGE = 8
} MR_LABEL_TYPE;

typedef struct {
	unsigned long long	mr_addr;	/* memory region address */
	unsigned long long	mr_size;	/* memory region length */
	unsigned char	mr_att;			/* memory region attributes */
	unsigned char	mr_label;		/* memory region label */
	unsigned short	mr_nodeid;		/* memory region NUMA node ID */
	char	reserved[4];			/* padding, future use */
} iplcb_map_reg_t;



/* structure for vm_saveatt() and vm_restatt() type services */

/* special large-page handle for use on 32-bit kernel */
typedef vmhandle_t      vmlpghandle_t;    /* large-page virtual memory handle */


/*
 * XPG4.2 requires structure elements to be defined such that they do not 
 * pollute the namespace.  Additional elements to these structures should
 * be added after the "#endif", and always prepended with "__".
 */

typedef struct label_t
{					/* kernel jump buffer */
	struct label_t   *prev;		/* chain to previous */
	ulong_t           iar;		/* resume address */
	ulong_t           stack;	/* stack pointer */
	ulong_t           toc;		/* toc pointer */
	ulong_t           cr;           /* non-volatile part of cr */
	ulong_t           intpri;	/* priority level of the process */
	ulong_t           reg[19];	/* non-volatile regs (13..31) */
} label_t;

typedef int32long64_t	ext_t;		/* extended parameters	        */ 



typedef char *		va_list;




typedef void * __ptr64;
typedef char * __cptr64;

/*
 * type definition for Unicode character code.
 */
typedef ushort_t	UniChar;
typedef uint_t		UTF32Char;

/*
 * shorthand type definitions for unsigned storage classes
 */
typedef	uchar_t		uchar;
typedef	ushort_t	ushort;
typedef	uint_t		uint;
typedef ulong_t		ulong;

typedef	struct { int r[1]; } *	physadr_t;
typedef	physadr_t	physadr;

/* typedefs for BSD unsigned things */
typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;

typedef	int	swblk_t;


/* The sigset structure must correspond to sigset_t in _POSIX_SOURCE above */
struct sigset	{
	unsigned long ss_set[4];
};

/* typedef for the File System Identifier (fsid) */
struct fsid {
	unsigned int	val[2];
};



struct fileid {			/* this is for servers only! */
	uint_t	fid_len;
	ino32_t	fid_ino;
	uint_t	fid_gen;
	char	fid_x[20 - (sizeof(ino32_t) + 2) - sizeof(uint_t)];
};


struct fid {
	uint_t	fid_len;
	char	fid_data[20];
};
typedef struct fid fid_t;

/* typedef for the File Handle (fhandle) */
struct fhandle {
	char x[32];		/* allows structure assignments */
};
typedef struct fhandle fhandle_t;

struct filehandle {			/* this is for servers only! */
	fsid_t		fh_fsid;		/* filesystem id */
	struct fileid	fh_fid;			/* file id */
};


/* structure and typedef for volume group IDs and physical volume IDs */
struct unique_id {
       __ulong32_t word1;
       __ulong32_t word2;
       __ulong32_t word3;
       __ulong32_t word4;
};
typedef struct unique_id unique_id_t;


typedef	long long  offset_t;		/* Unambiguous 64 bit file offset */

typedef long long ssize64_t;

typedef long long longlong_t;         
typedef unsigned long long u_longlong_t;

typedef unsigned int class_id_t;


/* some data types used by vdevices and others */
typedef uint_t          liobn_t;       /* Logical I/O Bus Number of a vdevice */
typedef unsigned long long unit_addr_t;   /* Unit address of the vdevice      */

typedef unsigned long long size64_t;    



/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/kernel/sys/times.h 1.10                                 */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1985,1992          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)44	1.10  src/bos/kernel/sys/times.h, sysproc, bos530 2/19/03 10:34:02 */

/*
 * COMPONENT_NAME: SYSPROC 
 *
 * FUNCTIONS:
 *
 * ORIGINS: 27 
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1992 
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */



/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/strict_stdtypes.h 1.1                       */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)28	1.1  src/bos/usr/include/strict_stdtypes.h, incstd, bos530 2/19/03 10:23:14 */
/* 
 * Restrict content from standard headers to the allowable typedefs.
 * This affects sys/types.h, inttypes.h, and stdint.h and any headers 
 * which include those headers.
 *
 * This file should be used in conjunction with <end_strict_stdtypes.h>, which
 * terminates the restricted environment.
 *
 * This has no effect if _ALL_SOURCE is defined.
 */




/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/end_strict_stdtypes.h 1.1                   */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)29	1.1  src/bos/usr/include/end_strict_stdtypes.h, incstd, bos530 2/19/03 10:23:29 */
/* 
 * This file should be used in conjunction with <strict_stdtypes.h>, which
 * defines a restricted content environment for standard headers.
 */

/* End restricted includes */


/*
 * POSIX requires that certain values be included in times.h.  It also
 * requires that when _POSIX_SOURCE is defined only those standard
 * specific values are present.  This header includes all the POSIX
 * required entries.
 */

/*
 * Structure returned by times()
 */
struct tms {
	clock_t	tms_utime;		/* user time */
	clock_t	tms_stime;		/* system time */
	clock_t	tms_cutime;		/* user time, children */
	clock_t	tms_cstime;		/* system time, children */
};

extern clock_t times(struct tms *);


/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos53Q src/bos/usr/include/time.h 1.29.3.36                            */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* COPYRIGHT International Business Machines Corp. 1985,2007              */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)72  1.29.3.36  src/bos/usr/include/time.h, libctime, bos53Q, q2008_05B1 3/21/07 05:31:46 */
/*
 * COMPONENT_NAME: (LIBCTIME) Standard C Library Time Management Functions
 *
 * FUNCTIONS:
 *
 * ORIGINS: 27,71
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1995
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

/*
 * (c) Copyright 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
 */




/*
 *
 *      The ANSI standard requires that certain values be in time.h.
 *      It also requires that if _ANSI_C_SOURCE is defined then ONLY these
 *      values are present.
 *
 *      This header includes all the ANSI required entries.  In addition
 *      other entries for the AIX system are included.
 *
 */

/* The following definitions are required to be in time.h by ANSI */






struct	tm {	/* see ctime(3) */
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
};

    extern size_t 	strftime(char *restrict, size_t, const char *restrict, const struct tm *restrict);
    extern clock_t 	clock(void);
    extern double 	difftime(time_t, time_t);
    extern time_t 	mktime(struct tm *);
    extern time_t 	time(time_t *);
    extern char 	*asctime(const struct tm *);
    extern char 	*ctime(const time_t *);
    extern struct tm *gmtime(const time_t *);
    extern struct tm *localtime(const time_t *);

/* REENTRANT FUNCTIONS */
    extern char		*asctime_r(const struct tm *restrict, char *restrict);
    extern char		*ctime_r(const time_t *, char *);
    extern struct tm	*gmtime_r(const time_t *restrict, struct tm *restrict);
    extern struct tm	*localtime_r(const time_t *restrict, struct tm *restrict);

 
/*
 *   The following are values that have historically been in time.h.
 *
 *   They are NOT part of the ANSI defined time.h and therefore are
 *   not included when _ANSI_C_SOURCE is defined.
 *
 */


/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/strict_stdtypes.h 1.1                       */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)28	1.1  src/bos/usr/include/strict_stdtypes.h, incstd, bos530 2/19/03 10:23:14 */
/* 
 * Restrict content from standard headers to the allowable typedefs.
 * This affects sys/types.h, inttypes.h, and stdint.h and any headers 
 * which include those headers.
 *
 * This file should be used in conjunction with <end_strict_stdtypes.h>, which
 * terminates the restricted environment.
 *
 * This has no effect if _ALL_SOURCE is defined.
 */




/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/end_strict_stdtypes.h 1.1                   */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2003               */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)29	1.1  src/bos/usr/include/end_strict_stdtypes.h, incstd, bos530 2/19/03 10:23:29 */
/* 
 * This file should be used in conjunction with <strict_stdtypes.h>, which
 * defines a restricted content environment for standard headers.
 */

/* End restricted includes */


extern char *tzname[];

    extern void tzset(void);


    extern long timezone;
    extern int daylight;
    extern char         *strptime(const char *, const char *, struct tm *);

    extern int getdate_err;
    extern struct tm *getdate(const char *);

struct timespec {
    time_t tv_sec;         /* seconds */
    long   tv_nsec;        /* and nanoseconds */
};

struct itimerspec {
    struct  timespec it_interval; /* timer period */
    struct  timespec it_value;    /* expiration */
};


extern int clock_getres(clockid_t, struct timespec *);
extern int clock_gettime(clockid_t, struct timespec *);
extern int clock_settime(clockid_t, const struct timespec *);
extern int clock_getcpuclockid(pid_t, clockid_t *);
extern int nanosleep(const struct timespec *, struct timespec *);
extern int timer_create(clockid_t, void *restrict, timer_t *restrict);
extern int timer_delete(timer_t);
extern int timer_gettime(timer_t, struct itimerspec *);
extern int timer_getoverrun(timer_t);
extern int timer_settime(timer_t, int, const struct itimerspec *restrict, struct itimerspec *restrict);

extern int clock_nanosleep(clockid_t, int, const struct timespec*, struct timespec*);		
	

/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos530 src/bos/usr/include/stddef.h 1.12.1.5                           */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 1985,2002          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/* @(#)66  1.12.1.5  src/bos/usr/include/stddef.h, incstd, bos530 7/25/02 19:23:59 */

/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * ORIGINS: 27
 *
 */






/*
 *
 *      The ANSI standard requires that certain values be in stddef.h.
 *      It also requires that if _ANSI_C_SOURCE is defined then ONLY these
 *      values are present. This header includes all the ANSI required entries.
 *
 */


/*
 *	The following definitions are included in <sys/types.h>.  They
 *	are included in <stddef.h> to comply with ANSI.
 */









/*  Suggested default length of time/date buffer */
    extern unsigned char *NLctime(long *);
    extern unsigned char *NLasctime(struct tm *);
    extern char *NLstrtime(char *, size_t, const char *, const struct tm *);

/* time64 interfaces */
    extern char *ctime64(const time64_t *);
    extern struct tm *localtime64(const time64_t *);
    extern struct tm *gmtime64(const time64_t *);
    extern char *asctime64(const struct tm *);
    extern time64_t mktime64(struct tm *);
    extern double difftime64(time64_t, time64_t);
    extern char *ctime64_r(const time64_t *, char *);
    extern struct tm *localtime64_r(const time64_t *restrict, 
                                    struct tm *restrict);
    extern struct tm *gmtime64_r(const time64_t *restrict, 
                                 struct tm *restrict);
    extern char *asctime64_r(const struct tm *restrict, char *restrict);



/*
Kept the difference for reference.
=======
#if defined(__osf__) || defined(sysAIX)
#  include <time.h>
#else
#  include <limits.h>
>>>>>>> 1.1.2.2
*/

 /*
  The default is FORTRAN_UNDERSCORE_, but not explicitly used.
 */





 /*  Prototype: */

   void get_zeits_(double *zts);
   void get_ztick_(double *tic);

/*!REVISION HISTORY:
! 	12Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
! 	06Jul99 - J.W. Larson <jlarson@dao> - support for AIX platform
!EOP */

/*  Implementations: */

void get_zeits_(zts)
  double *zts;
{

  struct tms tm;
  double secs;

/*  secs=1./CLK_TCK;  */
  secs=1./1000000;

  zts[0]=times(&tm)*secs;
  zts[1]=tm.tms_utime*secs;
  zts[2]=tm.tms_stime*secs;
  zts[3]=tm.tms_cutime*secs;
  zts[4]=tm.tms_cstime*secs;

}

void get_ztick_(tic)
  double *tic;
{
/*  tic[0]=1./CLK_TCK;  */
  tic[0]=1./1000000;
}
