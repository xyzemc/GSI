/*==========================================================*/
/* Profiling Wrappers for MPI functions, Fortran and C.     */
/* Timing data is reported when MPI_Finalize is called.     */
/* Please e-mail corrections/errors to walkup@us.ibm.com.   */
/*==========================================================*/
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>
#include <sys/systemcfg.h>
#include <sys/resource.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/processor.h>
#include <sys/thread.h>
#include <omp.h>

/*----------------------------------------------------------*/
/* routine to get #cpus, etc., for binding tasks to cpus    */
/*----------------------------------------------------------*/
struct node_info {
                   int ncpus;
                   int tasks_per_node;
                   int rank_in_node;
                   int * all_numthreads;
                 };

void get_node_info(struct node_info *);

#define SORT_ASCENDING_ORDER 1
#define SORT_DESCENDING_ORDER -1

/*----------------------------------------------------------*/
/*    dimensions of arrays                                  */
/*----------------------------------------------------------*/
#define MAX_BINS 31
#define MAX_IDS  52

#define MAX_CALLERS 10000

#define MAX_PROFILE_BLOCKS 10000
#define FIFO_DEPTH 16

/*---------------------------------------------------------------*/
/* The default linker option is -bpT=0x100000000 in 64-bit mode. */
/* Return the instruction address as a 32-bit int for both 32-   */
/* and 64-bit modes, and save the ABI field for the viewer.      */
/*---------------------------------------------------------------*/
#ifdef __64BIT__
#define ADDRESS_OFFSET_64BIT 0x100000000L 
static int ABI=64;
#else
static int ABI=32;
#endif
 
#define F_ADDRESS 0xffffffff

/*----------------------------------------------------------*/
/*    define a structure to hold trace events               */
/*----------------------------------------------------------*/
static int max_events=50000;
static int event_buffer_overflow = 0;

struct eventstruct {
                     double tbeg;
                     double tend;
                     int taskid;
                     int eventid;
                     int src;
                     int dest;
                     int bytes;
                     int parent;
                     int grandparent;
                     int abi;
                   };

struct eventstruct * event;

/*----------------------------------------------------------*/
/*    integer IDs for MPI functions                         */
/*----------------------------------------------------------*/
#define COMM_SIZE_ID         0
#define COMM_RANK_ID         1
#define SEND_ID              2
#define SSEND_ID             3 
#define RSEND_ID             4 
#define BSEND_ID             5
#define ISEND_ID             6
#define ISSEND_ID            7
#define IRSEND_ID            8
#define IBSEND_ID            9
#define SEND_INIT_ID        10
#define SSEND_INIT_ID       11
#define RSEND_INIT_ID       12
#define BSEND_INIT_ID       13
#define RECV_INIT_ID        14
#define RECV_ID             15
#define IRECV_ID            16
#define SENDRECV_ID         17
#define SENDRECV_REPLACE_ID 18
#define BUFFER_ATTACH_ID    19
#define BUFFER_DETACH_ID    20
#define PROBE_ID            21
#define IPROBE_ID           22
#define TEST_ID             23
#define TESTANY_ID          24
#define TESTALL_ID          25
#define TESTSOME_ID         26
#define WAIT_ID             27
#define WAITANY_ID          28
#define WAITALL_ID          29
#define WAITSOME_ID         30
#define START_ID            31
#define STARTALL_ID         32
#define BCAST_ID            33
#define BARRIER_ID          34
#define GATHER_ID           35
#define GATHERV_ID          36
#define SCATTER_ID          37
#define SCATTERV_ID         38
#define SCAN_ID             39
#define ALLGATHER_ID        40
#define ALLGATHERV_ID       41
#define REDUCE_ID           42
#define ALLREDUCE_ID        43
#define REDUCE_SCATTER_ID   44
#define ALLTOALL_ID         45
#define ALLTOALLV_ID        46

#define IREDUCE_ID          47
#define IBCAST_ID           48
#define ISCATTERV_ID        49
#define IALLREDUCE_ID       50
#define IALLTOALLV_ID       51

/*----------------------------------------------------------*/
/*    elapsed-time timing functions                         */
/*----------------------------------------------------------*/
#define WTIME(TB1) read_real_time(&(TB1), TIMEBASE_SZ)
#define TCONV(TB1) tconv*(fhigh*((double) (TB1).tb_high) + 1.0e-9*((double) (TB1).tb_low))

/*----------------------------------------------------------*/
/*    variables with file scope                             */
/*----------------------------------------------------------*/
static int taskid, ntasks;
static double tconv, fhigh;
static double elapsed_time, elapsed_time_initial;
static double time_init = 0.0;
static int event_number = 0;
static struct timebasestruct TB_INIT;

static int * node_ranks_in_comm_world = NULL;
static int tasks_in_node = 0;

static int traceback_level = 0;
static int swap_bytes = 1;
static int trace_events = 0;
static int save_all_tasks = 1;
static int trace_max_rank = 256;

static int collect_summary = 1;
static int first_summary_start = 1;
static int first_trace_start = 1;

static long max_memory;
static int context_switches, context_switches_initial;
static double elapsed_time_initial;
static double user_time, user_time_initial;
static double system_time, system_time_initial;

static long long event_count[MAX_IDS];
static double total_time[MAX_IDS];
static double total_bytes[MAX_IDS];

static char label[MAX_IDS][80];

static long long bin_count[MAX_IDS][MAX_BINS];
static double bin_bytes[MAX_IDS][MAX_BINS];
static double bin_time[MAX_IDS][MAX_BINS];

static long long on_node_bin_count[MAX_IDS][MAX_BINS];
static double on_node_bin_bytes[MAX_IDS][MAX_BINS];
static double on_node_bin_time[MAX_IDS][MAX_BINS];

static long long on_node_event_count[MAX_IDS];
static double on_node_total_time[MAX_IDS];
static double on_node_total_bytes[MAX_IDS];

static MPI_Group world_group = MPI_GROUP_EMPTY;

static int wrapper_init = 0;

static int call_graph = 0;

static int profile_block = 0;
static int profile_by_call_site = 0;
static int profile_by_node = 0;
static int profile_fifo[FIFO_DEPTH];
static long long profile_call_count[MAX_PROFILE_BLOCKS][MAX_IDS];;
static int profile_callsite[MAX_PROFILE_BLOCKS];
static double profile_elapsed_time[MAX_PROFILE_BLOCKS];
static double profile_callsite_time[MAX_PROFILE_BLOCKS][MAX_IDS];

static int smt_packed = 0;

static int num_callers = 0;
static int callgraph_level = 2;
static long long callgraph_count[MAX_CALLERS][MAX_IDS];
static double callgraph_time[MAX_CALLERS][MAX_IDS];
static double caller_time[MAX_CALLERS];
static char callgraph_parent[MAX_CALLERS][128];


/*----------------------------------------------------------*/
/*    function prototypes                                   */
/*----------------------------------------------------------*/
static void LogEvent(int, struct timebasestruct, struct timebasestruct, int, int, int, MPI_Comm);
static void write_tracefile(FILE *, struct eventstruct *, int);
static void reverse_byte_order(char *, char *, int);
static void swap8(char * in, char * out);
static void swap4(char * in, char * out);
static void get_parent(int, int *, int *);
static void get_call_site(int, int *, int *);
static int index_from_address(int);
static void print_profile_by_call_site(FILE *);

void trace_start(void);
void trace_start_(void);
void trace_stop(void);
void trace_stop_(void);

static void initialize_summary_data(void);
static void stop_timers(void);

/*----------------------------------------------------------*/
/*    fortran profiling entry points                        */
/*----------------------------------------------------------*/
void pmpi_init(int *);
void pmpi_init_thread(int *, int *, int *);
void pmpi_finalize(int *);
void pmpi_comm_rank(int *, int *, int *);
void pmpi_comm_size(int *, int *, int *);
void pmpi_get_count(int *, int *, int *, int *);
void pmpi_send(void *, int *, int *, int *, int *, int *, int *);
void pmpi_ssend(void *, int *, int *, int *, int *, int *, int *);
void pmpi_rsend(void *, int *, int *, int *, int *, int *, int *);
void pmpi_bsend(void *, int *, int *, int *, int *, int *, int *);
void pmpi_isend(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_issend(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_irsend(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_ibsend(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_send_init(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_ssend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_rsend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_bsend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_recv_init(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_recv(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_irecv(void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_sendrecv(void *, int *, int *, int *, int *, 
                   void *, int *, int *, int *, int *, int *, MPI_Status *, int *);
void pmpi_sendrecv_replace(void *, int *, int *, int *, int *, 
                          int *, int *, int *, MPI_Status *, int *);
void pmpi_buffer_attach(void *, int *, int *);
void pmpi_buffer_detach(void *, int *, int *);
void pmpi_probe(int *, int *, int *, MPI_Status *, int *);
void pmpi_iprobe(int *, int *, int *, int *, MPI_Status *, int *);
void pmpi_test(int *, int *, MPI_Status *, int *);
void pmpi_testany(int *, int *, int*, int *, MPI_Status *, int *);
void pmpi_testall(int *, int*, int *, MPI_Status *, int *);
void pmpi_testsome(int *, int *, int *, int *, MPI_Status *, int *);
void pmpi_wait(int *, MPI_Status *, int *);
void pmpi_waitany(int *, int *, int *, MPI_Status *, int *);
void pmpi_waitall(int *, int *, MPI_Status *, int *);
void pmpi_waitsome(int *, int *, int *, int *, MPI_Status *, int *);
void pmpi_start(int *, int *);
void pmpi_startall(int *, int *, int *);
void pmpi_bcast(void *, int *, int *, int *, int *, int *);
void pmpe_ibcast(void *, int *, int *, int *, int *, int *, int *);
void pmpi_barrier(int *, int *);
void pmpi_reduce(void *, void *, int *, int *, int *, int *, int *, int *);
void pmpe_ireduce(void *, void *, int *, int *, int *, int *, int *, int *, int *);
void pmpi_allreduce(void *, void *, int *, int *, int *, int *, int *);
void pmpe_iallreduce(void *, void *, int *, int *, int *, int *, int *, int *);
void pmpi_reduce_scatter(void *, void *, int *, int *, int *, int *, int *);
void pmpi_gather(void *, int *, int *, void *, int *, int *, int *, int *, int *);
void pmpi_gatherv(void *, int *, int *, void *, int *, int *, int *,int *, int *, int *);
void pmpi_scan(void *, void *, int *, int *, int *, int *, int *);
void pmpi_allgather(void *, int *, int *, void *, int *, int *, int *, int *);
void pmpi_allgatherv(void *, int *, int *, void *, int *, int *,int *, int *, int *);
void pmpi_scatter(void *, int *, int *, void *, int *, int *, int *, int *, int *);
void pmpi_scatterv(void *, int *, int *, int *, void *, int *, int *, int *, int *, int *);
void pmpe_iscatterv(void *, int *, int *, int *, void *, int *, int *, int *, int *, int *, int *);
void pmpi_alltoall(void *, int *, int *, void *, int *, int *, int *, int *);
void pmpi_alltoallv(void *, int *, int *, int *, void *, int *, int *,int *, int *, int *);
void pmpe_ialltoallv(void *, int *, int *, int *, void *, int *, int *,int *, int *, int *, int *);

void mpitrace_sortx(double *, int, int *, int);

void mpitrace_caller(int, char *);

#ifdef HPM
/*----------------------------------------------------------------*/
/* add performance monitor counters from mpi_init to mpi_finalize */
/*----------------------------------------------------------------*/
static int pmgroup;
static int pmgroup_list[160];
void mpihpm_start(int);
void mpihpm_stop(void);
void mpihpm_print(double,double,double,double,int);
void mpihpm_print_old_format(FILE *);

static int power5_std_list[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 16, 17,
                                 18, 19, 20, 21, 24, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 38,
                                 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 78, 80, 81, 82,
                                 83, 84, 85, 87, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,100,101,
                                109,111,128,129,131,132};

static int power5_plus_list[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 17,
                                  18, 19, 20, 21, 22, 25, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
                                  39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 83,
                                  84, 86, 87, 88, 90, 93, 94, 95, 96, 97, 98, 99,100,101,102,103,
                                 104,112,114,119,120,121,122,123,124,125,133,134,136,137,150};
#endif

/*------------------------------------------*/
/* Fortran entry points with no underscores */
/*------------------------------------------*/
#include "fortran_wrappers.c"

/*------------------------------------------*/
/* Fortran entry points with one underscore */
/*------------------------------------------*/
#define mpi_init             mpi_init_
#define mpi_init_thread      mpi_init_thread_
#define mpi_finalize         mpi_finalize_
#define mpi_comm_rank        mpi_comm_rank_
#define mpi_comm_size        mpi_comm_size_
#define mpi_send             mpi_send_
#define mpi_ssend            mpi_ssend_
#define mpi_rsend            mpi_rsend_
#define mpi_bsend            mpi_bsend_
#define mpi_isend            mpi_isend_
#define mpi_issend           mpi_issend_
#define mpi_irsend           mpi_irsend_
#define mpi_ibsend           mpi_ibsend_
#define mpi_send_init        mpi_send_init_
#define mpi_ssend_init       mpi_ssend_init_
#define mpi_rsend_init       mpi_rsend_init_
#define mpi_bsend_init       mpi_bsend_init_
#define mpi_recv_init        mpi_recv_init_
#define mpi_recv             mpi_recv_
#define mpi_irecv            mpi_irecv_
#define mpi_sendrecv         mpi_sendrecv_
#define mpi_sendrecv_replace mpi_sendrecv_replace_
#define mpi_buffer_attach    mpi_buffer_attach_
#define mpi_buffer_detach    mpi_buffer_detach_
#define mpi_probe            mpi_probe_
#define mpi_iprobe           mpi_iprobe_
#define mpi_test             mpi_test_
#define mpi_testany          mpi_testany_
#define mpi_testall          mpi_testall_
#define mpi_testsome         mpi_testsome_
#define mpi_wait             mpi_wait_
#define mpi_waitany          mpi_waitany_
#define mpi_waitall          mpi_waitall_
#define mpi_waitsome         mpi_waitsome_
#define mpi_start            mpi_start_
#define mpi_startall         mpi_startall_
#define mpi_bcast            mpi_bcast_
#define mpi_barrier          mpi_barrier_
#define mpi_reduce           mpi_reduce_
#define mpi_allreduce        mpi_allreduce_
#define mpi_reduce_scatter   mpi_reduce_scatter_
#define mpi_gather           mpi_gather_
#define mpi_gatherv          mpi_gatherv_
#define mpi_scan             mpi_scan_
#define mpi_allgather        mpi_allgather_
#define mpi_allgatherv       mpi_allgatherv_
#define mpi_scatter          mpi_scatter_
#define mpi_scatterv         mpi_scatterv_
#define mpi_alltoall         mpi_alltoall_
#define mpi_alltoallv        mpi_alltoallv_

#define mpe_ireduce          mpe_ireduce_
#define mpe_ibcast           mpe_ibcast_
#define mpe_iscatterv        mpe_iscatterv_
#define mpe_iallreduce       mpe_iallreduce_
#define mpe_ialltoallv       mpe_ialltoallv_

#include "fortran_wrappers.c"

/*----------------------------------------------------------*/
/*    Function to log events                                */
/*----------------------------------------------------------*/
static void LogEvent(int id, struct timebasestruct TB1, struct timebasestruct TB2, 
                     int src, int dest, int bytes, MPI_Comm comm)
{
   int i, k, bin, limit;
   double time1, time2;
   double tbeg, tend, timediff;
   int call_site, parent, grandparent;
   int nchars, match;
   char * ptr;
   char this_caller[128];
   MPI_Group group;
   int dest_is_known, src_is_known;
   int dest_on_node, src_on_node;
   int world_dest, world_src;
   int on_node;

   if (collect_summary == 0) return;

   time1 = TCONV(TB1);
   time2 = TCONV(TB2);
 
   tbeg = time1 - time_init;
   tend = time2 - time_init;

   timediff = tend - tbeg;

   if (profile_by_call_site)
   {
     get_parent(traceback_level, &parent, &grandparent);

     if (parent != F_ADDRESS)
     {
        k = index_from_address(parent);
        profile_call_count[k][id]++;
        profile_callsite_time[k][id] += timediff;
        profile_elapsed_time[k] += timediff;
     }

   }

   /*------------------------------------------------*/
   /* save trace records if event tracing is enabled */
   /*------------------------------------------------*/
   if ( trace_events && (event_number < max_events) )
   {
     get_parent(traceback_level, &parent, &grandparent);
     event[event_number].tbeg        = tbeg;
     event[event_number].tend        = tend;
     event[event_number].taskid      = taskid;
     event[event_number].eventid     = id;
     event[event_number].src         = src;
     event[event_number].dest        = dest;
     event[event_number].bytes       = bytes;
     event[event_number].parent      = parent;
     event[event_number].grandparent = grandparent;
     event[event_number].abi         = ABI;
     event_number ++;
     if (event_number == max_events) event_buffer_overflow = 1;
   }

   /*---------------------------------------------------*/
   /* put messages into bins, at power-of-two intervals */
   /*---------------------------------------------------*/
   if (bytes == 0)
   {
       bin = 0; 
       bin_count[id][bin] ++;
       bin_bytes[id][bin] += (double) bytes;
       bin_time[id][bin] += timediff;
   }

   if (bytes > 0)
   {
       bin = 1; limit = 4;
       while (bytes > limit) 
       {
          limit *= 2;
          bin ++;
       }
       bin_count[id][bin] ++;
       bin_bytes[id][bin] += (double) bytes;
       bin_time[id][bin] += timediff;
   }

   event_count[id] ++;
   total_time[id] += timediff;
   if (bytes >= 0) total_bytes[id] += (double) bytes;

   /*-------------------------------------------*/
   /* account for communication that is on-node */
   /*-------------------------------------------*/
   if (profile_by_node)
   {

      if ( (dest >= 0) && (dest < ntasks) ) dest_is_known = 1;
      else                                  dest_is_known = 0;

      if ( (src  >= 0) && (src  < ntasks) ) src_is_known = 1;
      else                                  src_is_known = 0;

      /*--------------------------------------------*/
      /* translate ranks to comm_world if necessary */
      /*--------------------------------------------*/
      if ( dest_is_known && ( comm != MPI_COMM_WORLD ) )
      {
          MPI_Comm_group(comm, &group);
          MPI_Group_translate_ranks(group, 1, &dest, world_group, &world_dest);
      }
      else world_dest = dest;

      if ( src_is_known && ( comm != MPI_COMM_WORLD ) )
      {
          MPI_Comm_group(comm, &group);
          MPI_Group_translate_ranks(group, 1, &src, world_group, &world_src);
      }
      else world_src = src;

      dest_on_node = 0;

      if (dest_is_known)
      {
         for (i=0; i<tasks_in_node; i++)
         {
             if (world_dest == node_ranks_in_comm_world[i])
             {
                dest_on_node = 1;
                break;
             }
         }
      }

      src_on_node = 0;

      if (src_is_known)
      {
         for (i=0; i<tasks_in_node; i++)
         {
             if (world_src == node_ranks_in_comm_world[i])
             {
                src_on_node = 1;
                break;
             }
         }
      }
 
      on_node = 0;

      if (dest_is_known && !src_is_known && dest_on_node) on_node = 1;

      if (src_is_known && !dest_is_known && src_on_node)  on_node = 1;

      if (dest_is_known && src_is_known && dest_on_node && src_on_node) on_node = 1;


      if (on_node)
      {
          if (bytes == 0)
          {
              bin = 0; 
              on_node_bin_count[id][bin] ++;
              on_node_bin_bytes[id][bin] += (double) bytes;
              on_node_bin_time[id][bin] += timediff;
          }

          if (bytes > 0)
          {
              bin = 1; limit = 4;
              while (bytes > limit) 
              {
                 limit *= 2;
                 bin ++;
              }
              on_node_bin_count[id][bin] ++;
              on_node_bin_bytes[id][bin] += (double) bytes;
              on_node_bin_time[id][bin] += timediff;
          }
   
          on_node_event_count[id] ++;
          on_node_total_time[id] += timediff;
          if (bytes >= 0) on_node_total_bytes[id] += (double) bytes;
      }
   }

   if (call_graph)
   {
      if (num_callers < MAX_CALLERS)
      {
          mpitrace_caller(callgraph_level, this_caller);

          /* if the traceback returns garbage, call it unknown */
          nchars = strlen(this_caller);
          for (i=0; i<nchars; i++)
          {
              if (!isascii(this_caller[i]))
              {
                  strcpy(this_caller, "unknown");
                  break;
              }
          }

          match = 0;
          for (i=num_callers-1; i>=0; i--)
          {
              if (0 == strcmp(callgraph_parent[i], this_caller))
              {
                  match = 1;
                  break;
              }
          }
    
          if (match == 0)
          {
              i = num_callers;
              ptr = strcpy(callgraph_parent[i], this_caller);
              if (ptr == NULL) callgraph_parent[i][0] = '\0';
              num_callers ++;
          }
    
          callgraph_time[i][id] += timediff;
          callgraph_count[i][id] ++;
          caller_time[i] += timediff;
      }
   }

   return;
}


/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Init                               */
/*----------------------------------------------------------*/
int MPI_Init(int * argc, char *** argv)
{
   int i, id, rc, rcinit, bin;
   double tb_top, tb_bot;
   struct timeval TV;
   struct rusage RU;
   char * ptr;
   char * list_ptr;
   char delimiters[] = {" ,"};
   int num_groups;
   int ncpus, tasks_per_node, rank_in_node;
   int thread_id, total_threads, *all_numthreads;
   int cpu_base, cpu_inc, my_base, my_cpu;
   struct node_info node_info;
   int bind_flag;

#include "init_part1.c"

   rcinit = PMPI_Init(argc, argv);

#include "init_part2.c"

   return rcinit;
}


/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Init_thread                        */
/*----------------------------------------------------------*/
int MPI_Init_thread(int * argc, char *** argv, int required, int * provided)
{
   int i, id, rc, rcinit, bin;
   double tb_top, tb_bot;
   struct timeval TV;
   struct rusage RU;
   char * ptr;
   char * list_ptr;
   char delimiters[] = {" ,"};
   int num_groups;
   int ncpus, tasks_per_node, rank_in_node;
   int thread_id, total_threads, *all_numthreads;
   int cpu_base, cpu_inc, my_base, my_cpu;
   struct node_info node_info;
   int bind_flag;

#include "init_part1.c"

   rcinit = PMPI_Init_thread(argc, argv, required, provided);

#include "init_part2.c"

   return rcinit;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Comm_rank                          */
/*----------------------------------------------------------*/
int MPI_Comm_rank(MPI_Comm comm, int * id)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Comm_rank(comm, id);
   WTIME(TB2);

   LogEvent(COMM_RANK_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Comm_size                          */
/*----------------------------------------------------------*/
int MPI_Comm_size(MPI_Comm comm, int * ptasks)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Comm_size(comm, ptasks);
   WTIME(TB2);

   LogEvent(COMM_SIZE_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}


/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Send                               */
/*----------------------------------------------------------*/
int MPI_Send(void * sbuf, int count, MPI_Datatype type, int dest, 
             int tag, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Send(sbuf, count, type, dest, tag, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Ssend                              */
/*----------------------------------------------------------*/
int MPI_Ssend(void * sbuf, int count, MPI_Datatype type, int dest, 
              int tag, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Ssend(sbuf, count, type, dest, tag, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Rsend                              */
/*----------------------------------------------------------*/
int MPI_Rsend(void * sbuf, int count, MPI_Datatype type, int dest, 
              int tag, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Rsend(sbuf, count, type, dest, tag, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(RSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Bsend                              */
/*----------------------------------------------------------*/
int MPI_Bsend(void * sbuf, int count, MPI_Datatype type, int dest, 
              int tag, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Bsend(sbuf, count, type, dest, tag, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(BSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Isend                              */
/*----------------------------------------------------------*/
int MPI_Isend(void * sbuf, int count, MPI_Datatype type, int dest, 
              int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Isend(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(ISEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Issend                             */
/*----------------------------------------------------------*/
int MPI_Issend(void * sbuf, int count, MPI_Datatype type, int dest, 
               int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Issend(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(ISSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Irsend                             */
/*----------------------------------------------------------*/
int MPI_Irsend(void * sbuf, int count, MPI_Datatype type, int dest, 
               int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Irsend(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IRSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Ibsend                             */
/*----------------------------------------------------------*/
int MPI_Ibsend(void * sbuf, int count, MPI_Datatype type, int dest, 
               int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Ibsend(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IBSEND_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Send_init                          */
/*----------------------------------------------------------*/
int MPI_Send_init(void * sbuf, int count, MPI_Datatype type, int dest, 
                  int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Send_init(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SEND_INIT_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Ssend_init                         */
/*----------------------------------------------------------*/
int MPI_Ssend_init(void * sbuf, int count, MPI_Datatype type, int dest, 
                   int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Ssend_init(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SSEND_INIT_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Rsend_init                         */
/*----------------------------------------------------------*/
int MPI_Rsend_init(void * sbuf, int count, MPI_Datatype type, int dest, 
                   int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Rsend_init(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(RSEND_INIT_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Bsend_init                         */
/*----------------------------------------------------------*/
int MPI_Bsend_init(void * sbuf, int count, MPI_Datatype type, int dest, 
                   int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Bsend_init(sbuf, count, type, dest, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(BSEND_INIT_ID, TB1, TB2, -1, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Recv_init                          */
/*----------------------------------------------------------*/
int MPI_Recv_init(void * sbuf, int count, MPI_Datatype type, int src, 
                  int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Recv_init(sbuf, count, type, src, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(RECV_INIT_ID, TB1, TB2, src, -1, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Recv                               */
/*----------------------------------------------------------*/
int MPI_Recv(void * rbuf, int count, MPI_Datatype type, int src, 
             int tag, MPI_Comm comm, MPI_Status * status)
{
   int rc, source, count_received, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Recv(rbuf, count, type, src, tag, comm, status);
   WTIME(TB2);

   if (status != MPI_STATUS_IGNORE)
   {
       PMPI_Get_count(status, type, &count_received);
       source = status->MPI_SOURCE;
   }
   else
   {
       count_received = count;
       source = src;
   }

   PMPI_Type_size(type, &bytes);
   bytes = count_received * bytes;

   LogEvent(RECV_ID, TB1, TB2, source, -1, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Irecv                              */
/*----------------------------------------------------------*/
int MPI_Irecv(void * rbuf, int count, MPI_Datatype type, int src, 
              int tag, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Irecv(rbuf, count, type, src, tag, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IRECV_ID, TB1, TB2, src, -1, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Sendrecv                           */
/*----------------------------------------------------------*/
int MPI_Sendrecv(void * sbuf, int scount, MPI_Datatype stype, int dest, int stag,
                 void * rbuf, int rcount, MPI_Datatype rtype, int src, int rtag,
                 MPI_Comm comm, MPI_Status * status)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Sendrecv(sbuf, scount, stype, dest, stag,
                      rbuf, rcount, rtype, src, rtag,
                      comm, status);
   WTIME(TB2);

   PMPI_Type_size(stype, &bytes);
   bytes = scount * bytes;

   LogEvent(SENDRECV_ID, TB1, TB2, src, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Sendrecv_replace                   */
/*----------------------------------------------------------*/
int MPI_Sendrecv_replace(void * buf, int count, MPI_Datatype type, int dest, int stag,
                         int src, int rtag, MPI_Comm comm, MPI_Status * status)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Sendrecv_replace(buf, count, type, dest, stag,
                              src, rtag, comm, status);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SENDRECV_REPLACE_ID, TB1, TB2, src, dest, bytes, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Buffer_attach                      */
/*----------------------------------------------------------*/
int MPI_Buffer_attach(void * buffer, int size)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Buffer_attach(buffer, size);
   WTIME(TB2);

   LogEvent(BUFFER_ATTACH_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Buffer_detach                      */
/*----------------------------------------------------------*/
int MPI_Buffer_detach(void * buffer, int * size)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Buffer_detach(buffer, size);
   WTIME(TB2);

   LogEvent(BUFFER_DETACH_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Probe                              */
/*----------------------------------------------------------*/
int MPI_Probe(int src, int tag, MPI_Comm comm, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Probe(src, tag, comm, status);
   WTIME(TB2);

   LogEvent(PROBE_ID, TB1, TB2, src, -1, -1, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Iprobe                             */
/*----------------------------------------------------------*/
int MPI_Iprobe(int src, int tag, MPI_Comm comm, int * flag, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Iprobe(src, tag, comm, flag, status);
   WTIME(TB2);

   LogEvent(IPROBE_ID, TB1, TB2, src, -1, -1, comm);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Test                               */
/*----------------------------------------------------------*/
int MPI_Test(MPI_Request * request, int * flag, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Test(request, flag, status);
   WTIME(TB2);

   LogEvent(TEST_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Testany                            */
/*----------------------------------------------------------*/
int MPI_Testany(int num, MPI_Request * req, int * indx, int * flag, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Testany(num, req, indx, flag, status);
   WTIME(TB2);

   LogEvent(TESTANY_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Testall                            */
/*----------------------------------------------------------*/
int MPI_Testall(int num, MPI_Request * req, int * flag, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Testall(num, req, flag, status);
   WTIME(TB2);

   LogEvent(TESTALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Testsome                           */
/*----------------------------------------------------------*/
int MPI_Testsome(int inum, MPI_Request * req, int * onum, int * ind, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Testsome(inum, req, onum, ind, status);
   WTIME(TB2);

   LogEvent(TESTSOME_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Wait                               */
/*----------------------------------------------------------*/
int MPI_Wait(MPI_Request * request, MPI_Status * status)
{
   int rc, src;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Wait(request, status);
   WTIME(TB2);

   /* note: status is only filled-out for recv requests */
   if (status != MPI_STATUS_IGNORE)
       src   = status->MPI_SOURCE;
   else
       src = -1;

   LogEvent(WAIT_ID, TB1, TB2, src, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Waitany                            */
/*----------------------------------------------------------*/
int MPI_Waitany(int num, MPI_Request * req, int * indx, MPI_Status * status)
{
   int rc, src;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Waitany(num, req, indx, status);
   WTIME(TB2);

   /* note: status is only filled-out for recv requests */
   if (status != MPI_STATUS_IGNORE)
       src   = status->MPI_SOURCE;
   else
       src = -1;

   LogEvent(WAITANY_ID, TB1, TB2, src, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Waitall                            */
/*----------------------------------------------------------*/
int MPI_Waitall(int num, MPI_Request * req, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Waitall(num, req, status);
   WTIME(TB2);

   LogEvent(WAITALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Waitsome                           */
/*----------------------------------------------------------*/
int MPI_Waitsome(int inum, MPI_Request * req, int * onum, int * ind, MPI_Status * status)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Waitsome(inum, req, onum, ind, status);
   WTIME(TB2);

   LogEvent(WAITSOME_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Start                              */
/*----------------------------------------------------------*/
int MPI_Start(MPI_Request * req)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Start(req);
   WTIME(TB2);

   LogEvent(START_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Startall                           */
/*----------------------------------------------------------*/
int MPI_Startall(int num, MPI_Request * req)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Startall(num, req);
   WTIME(TB2);

   LogEvent(STARTALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Bcast                              */
/*----------------------------------------------------------*/
int MPI_Bcast(void * data, int count, MPI_Datatype type, 
               int root, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Bcast(data, count, type, root, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(BCAST_ID, TB1, TB2, root, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPE_Ibcast                             */
/*----------------------------------------------------------*/
int MPE_Ibcast(void * data, int count, MPI_Datatype type, 
               int root, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPE_Ibcast(data, count, type, root, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IBCAST_ID, TB1, TB2, root, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Barrier                            */
/*----------------------------------------------------------*/
int MPI_Barrier(MPI_Comm comm)
{
   int rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Barrier(comm);
   WTIME(TB2);

   LogEvent(BARRIER_ID, TB1, TB2, -1, -1, -1, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Reduce                             */
/*----------------------------------------------------------*/
int MPI_Reduce(void * sbuf, void * rbuf, int count, MPI_Datatype type, 
               MPI_Op op, int root, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Reduce(sbuf, rbuf, count, type, op, root, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(REDUCE_ID, TB1, TB2, -1, root, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPE_Ireduce                            */
/*----------------------------------------------------------*/
int MPE_Ireduce(void * sbuf, void * rbuf, int count, MPI_Datatype type, 
                MPI_Op op, int root, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPE_Ireduce(sbuf, rbuf, count, type, op, root, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IREDUCE_ID, TB1, TB2, -1, root, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Allreduce                          */
/*----------------------------------------------------------*/
int MPI_Allreduce(void * sbuf, void * rbuf, int count, MPI_Datatype type, 
                  MPI_Op op, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Allreduce(sbuf, rbuf, count, type, op, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(ALLREDUCE_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPE_Iallreduce                         */
/*----------------------------------------------------------*/
int MPE_Iallreduce(void * sbuf, void * rbuf, int count, MPI_Datatype type, 
                   MPI_Op op, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPE_Iallreduce(sbuf, rbuf, count, type, op, comm, req);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(IALLREDUCE_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Reduce_scatter                     */
/*----------------------------------------------------------*/
int MPI_Reduce_scatter(void * sbuf, void * rbuf, int * counts, MPI_Datatype type, 
                       MPI_Op op, MPI_Comm comm)
{
   int rc, i, bytes, num, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Reduce_scatter(sbuf, rbuf, counts, type, op, comm);
   WTIME(TB2);

   PMPI_Comm_size(comm, &tasks);
   PMPI_Type_size(type, &bytes);

   num = 0;
   for (i=0; i<tasks; i++) num += counts[i];
   bytes = num * bytes;

   LogEvent(REDUCE_SCATTER_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Gather                             */
/*----------------------------------------------------------*/
int MPI_Gather(void * sbuf, int scount, MPI_Datatype stype,
               void * rbuf, int rcount, MPI_Datatype rtype,
               int root, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Gather(sbuf, scount, stype, rbuf, rcount, rtype, root, comm);
   WTIME(TB2);

   PMPI_Type_size(stype, &bytes);
   bytes = scount * bytes;

   LogEvent(GATHER_ID, TB1, TB2, -1, root, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Gatherv                            */
/*----------------------------------------------------------*/
int MPI_Gatherv(void * sbuf, int scount, MPI_Datatype stype,
                void * rbuf, int * rcounts, int * rdisp, MPI_Datatype rtype,
                int root, MPI_Comm comm)
{
   int rc, bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Gatherv(sbuf, scount, stype, rbuf, rcounts, rdisp, rtype, root, comm);
   WTIME(TB2);

   PMPI_Type_size(stype, &bytes);
   bytes = scount * bytes;

   LogEvent(GATHERV_ID, TB1, TB2, -1, root, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Scan                               */
/*----------------------------------------------------------*/
int MPI_Scan(void * sbuf, void * rbuf, int count, MPI_Datatype type, 
             MPI_Op op, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Scan(sbuf, rbuf, count, type, op, comm);
   WTIME(TB2);

   PMPI_Type_size(type, &bytes);
   bytes = count * bytes;

   LogEvent(SCAN_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Allgather                          */
/*----------------------------------------------------------*/
int MPI_Allgather(void * sbuf, int scount, MPI_Datatype stype,
                  void * rbuf, int rcount, MPI_Datatype rtype,
                  MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Allgather(sbuf, scount, stype, rbuf, rcount, rtype, comm);
   WTIME(TB2);

   PMPI_Type_size(rtype, &bytes);
   bytes = rcount * bytes;

   LogEvent(ALLGATHER_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Allgatherv                         */
/*----------------------------------------------------------*/
int MPI_Allgatherv(void * sbuf, int scount, MPI_Datatype stype,
                   void * rbuf, int * rcounts, int * rdisp, MPI_Datatype rtype,
                   MPI_Comm comm)
{
   int rc, bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Allgatherv(sbuf, scount, stype, rbuf, rcounts, rdisp, rtype, comm);
   WTIME(TB2);

   PMPI_Comm_rank(comm, &id);
   PMPI_Type_size(rtype, &bytes);
   bytes = rcounts[id] * bytes;

   LogEvent(ALLGATHERV_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Scatter                            */
/*----------------------------------------------------------*/
int MPI_Scatter(void * sbuf, int scount, MPI_Datatype stype,
                void * rbuf, int rcount, MPI_Datatype rtype,
                int root, MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Scatter(sbuf, scount, stype, rbuf, rcount, rtype, root, comm);
   WTIME(TB2);

   PMPI_Type_size(rtype, &bytes);
   bytes = rcount * bytes;

   LogEvent(SCATTER_ID, TB1, TB2, root, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Scatterv                           */
/*----------------------------------------------------------*/
int MPI_Scatterv(void * sbuf, int * scounts, int * sdisp, MPI_Datatype stype,
                 void * rbuf, int rcount, MPI_Datatype rtype,
                 int root, MPI_Comm comm)
{
   int rc, bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Scatterv(sbuf, scounts, sdisp, stype, rbuf, rcount, rtype, root, comm);
   WTIME(TB2);

   PMPI_Type_size(rtype, &bytes);
   bytes = rcount * bytes;

   LogEvent(SCATTERV_ID, TB1, TB2, root, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPE_Iscatterv                          */
/*----------------------------------------------------------*/
int MPE_Iscatterv(void * sbuf, int * scounts, int * sdisp, MPI_Datatype stype,
                  void * rbuf, int rcount, MPI_Datatype rtype,
                  int root, MPI_Comm comm, MPI_Request * req)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPE_Iscatterv(sbuf, scounts, sdisp, stype, rbuf, rcount, rtype, root, comm, req);
   WTIME(TB2);

   PMPI_Type_size(rtype, &bytes);
   bytes = rcount * bytes;

   LogEvent(ISCATTERV_ID, TB1, TB2, root, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Alltoall                           */
/*----------------------------------------------------------*/
int MPI_Alltoall(void * sbuf, int scount, MPI_Datatype stype,
                 void * rbuf, int rcount, MPI_Datatype rtype,
                 MPI_Comm comm)
{
   int rc, bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Alltoall(sbuf, scount, stype, rbuf, rcount, rtype, comm);
   WTIME(TB2);

   PMPI_Type_size(stype, &bytes);
   bytes = scount * bytes;

   LogEvent(ALLTOALL_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Alltoallv                          */
/*----------------------------------------------------------*/
int MPI_Alltoallv(void * sbuf, int * scounts, int * sdisp, MPI_Datatype stype,
                  void * rbuf, int * rcounts, int * rdisp, MPI_Datatype rtype,
                  MPI_Comm comm)
{
   int rc, i, count, bytes, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPI_Alltoallv(sbuf, scounts, sdisp, stype, rbuf, rcounts, rdisp, rtype, comm);
   WTIME(TB2);

   PMPI_Comm_size(comm, &tasks);
   PMPI_Type_size(stype, &bytes);

   count = 0;
   for (i=0; i<tasks; i++) count += scounts[i];
   bytes = (count * bytes) / tasks;

   LogEvent(ALLTOALLV_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}

/*----------------------------------------------------------*/
/*    wrapper for C: MPE_Ialltoallv                         */
/*----------------------------------------------------------*/
int MPE_Ialltoallv(void * sbuf, int * scounts, int * sdisp, MPI_Datatype stype,
                   void * rbuf, int * rcounts, int * rdisp, MPI_Datatype rtype,
                   MPI_Comm comm, MPI_Request * req)
{
   int rc, i, count, bytes, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   rc = PMPE_Ialltoallv(sbuf, scounts, sdisp, stype, rbuf, rcounts, rdisp, rtype, comm, req);
   WTIME(TB2);

   PMPI_Comm_size(comm, &tasks);
   PMPI_Type_size(stype, &bytes);

   count = 0;
   for (i=0; i<tasks; i++) count += scounts[i];
   bytes = (count * bytes) / tasks;

   LogEvent(IALLTOALLV_ID, TB1, TB2, -1, -1, bytes, comm); 
   return rc;
}


/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Finalize                           */
/*----------------------------------------------------------*/
int MPI_Finalize(void)
{
#include "finalize.c"

   rc = PMPI_Finalize();

   return rc;
}

/*==========================================================*/
/* routine to get #cpus, etc., for binding tasks to cpus    */
/*==========================================================*/
void get_node_info(struct node_info *nodeinfo)
{
  int i, rc;
  int myid;
  int ncpus, tasks_per_node, color, rank_in_node;
  int thread, numthreads, * all_numthreads;
  MPI_Comm node_comm;
  char host[80];
  char * names;
  char * ptr;

  ncpus = _system_configuration.ncpus;

  rc = PMPI_Comm_rank(MPI_COMM_WORLD, &myid);

  /*--------------------------------------------*/
  /* each task gets the hostname for every task */
  /*--------------------------------------------*/
  names = (char *) malloc(ntasks*sizeof(host));
  gethostname(host, sizeof(host));
  for (i=0; i<sizeof(host); i++)
  {
    if (host[i] == '.')
    {
      host[i] = '\0';
      break;
    }
  }

  rc = PMPI_Allgather(host,  sizeof(host), MPI_BYTE,
                      names, sizeof(host), MPI_BYTE, MPI_COMM_WORLD);

  /*-------------------------------------------------------*/
  /* each task determines the number of tasks on it's node */
  /* and it's local rank among those tasks                 */
  /*-------------------------------------------------------*/
  tasks_per_node = 0;


  for (i=0; i<ntasks; i++)
  {
    ptr = names + i*sizeof(host);
    if (strcmp(host, ptr) == 0)
    {
      if (myid == i) rank_in_node = tasks_per_node;
      tasks_per_node++;

      if (tasks_per_node == 1) color = i;
    }
  }

  /*--------------------------------------------*/
  /* get the number of threads for this process */
  /*--------------------------------------------*/
#ifdef _OPENMP
#pragma OMP parallel private(thread)
{
  thread = omp_get_thread_num();
  if (thread == 0) numthreads = omp_get_num_threads();
}
#else
  numthreads = 1;
#endif

  /*---------------------------------------------------------*/
  /* setup a communicator for all tasks that share this node */
  /* and find out how many threads belong to each local rank */
  /*---------------------------------------------------------*/
  PMPI_Comm_split(MPI_COMM_WORLD, color, rank_in_node, &node_comm);

  all_numthreads = (int *) malloc(tasks_per_node*sizeof(int));

  PMPI_Allgather(&numthreads,    1, MPI_INT,
                 all_numthreads, 1, MPI_INT, node_comm);

  /*------------------------------------------------------------------*/
  /* save a list of all ranks in mpi_comm_world that are on this node */
  /*------------------------------------------------------------------*/
  node_ranks_in_comm_world = (int *) malloc(tasks_per_node*sizeof(int));

  PMPI_Allgather(&myid,                    1, MPI_INT,
                 node_ranks_in_comm_world, 1, MPI_INT, node_comm);

  nodeinfo->ncpus          = ncpus;
  nodeinfo->rank_in_node   = rank_in_node;
  nodeinfo->tasks_per_node = tasks_per_node;
  nodeinfo->all_numthreads = all_numthreads;

  free(names);

  return;

}



/*===========================================================*/
/* routine to start logging MPI events to a trace file       */
/*===========================================================*/
void trace_start(void)
{
    trace_events = 1;

    if (first_trace_start)
    {
        event_number = 0;
        first_trace_start = 0;
    }
}

void trace_start_(void)
{
    trace_events = 1;

    if (first_trace_start)
    {
        event_number = 0;
        first_trace_start = 0;
    }
}

/*===========================================================*/
/* routine to stop logging MPI events to a trace file        */
/*===========================================================*/
void trace_stop(void)
{
    trace_events = 0;
}

void trace_stop_(void)
{
    trace_events = 0;
}


/*===========================================================*/
/* routine to start collecting MPI summary data              */
/*===========================================================*/
void summary_start(void)
{
    collect_summary = 1;

    if (first_summary_start)
    {
       first_summary_start = 0;

       initialize_summary_data();
    }

}

void summary_start_(void)
{
    collect_summary = 1;

    if (first_summary_start)
    {
       first_summary_start = 0;

       initialize_summary_data();
    }

}

/*===========================================================*/
/* routine to stop collecting MPI cumulative data            */
/*===========================================================*/
void summary_stop(void)
{
    collect_summary = 0;

    stop_timers();
}

void summary_stop_(void)
{
    collect_summary = 0;

    stop_timers();
}

/*==========================================================*/
/* routine to reset initial times and cumulative statistics */
/*==========================================================*/
void initialize_summary_data(void)
{
    int id, bin;
    struct timeval TV;
    struct rusage RU;

    for (id=0; id<MAX_IDS; id++)
    {
       event_count[id] = 0LL;
       total_time[id]  = 0.0;
       total_bytes[id] = 0.0;

       for(bin=0; bin<MAX_BINS; bin++)
       {
          bin_count[id][bin] = 0LL;
          bin_bytes[id][bin] = 0.0;
          bin_time[id][bin]  = 0.0;
       }
    }

    getrusage(RUSAGE_SELF, &RU);

    user_time_initial = (double) (RU.ru_utime).tv_sec
              + 1.0e-6*((double) (RU.ru_utime).tv_usec);

    system_time_initial = (double) (RU.ru_stime).tv_sec
                + 1.0e-6*((double) (RU.ru_stime).tv_usec);

    context_switches_initial = RU.ru_nvcsw + RU.ru_nivcsw;

    gettimeofday(&TV, NULL);

    elapsed_time_initial= (double) TV.tv_sec + 1.0e-6 * ((double) TV.tv_usec);

}

/*==================================================*/
/* routine to stop timers for cumulative statistics */
/*==================================================*/
void stop_timers(void)
{
    struct timeval TV;
    struct rusage RU;

    getrusage(RUSAGE_SELF, &RU);

    user_time = (double) (RU.ru_utime).tv_sec
              + 1.0e-6*((double) (RU.ru_utime).tv_usec)
              - user_time_initial;

    system_time = (double) (RU.ru_stime).tv_sec
                + 1.0e-6*((double) (RU.ru_stime).tv_usec)
                - system_time_initial;

    max_memory = RU.ru_maxrss;

    context_switches = RU.ru_nvcsw + RU.ru_nivcsw
                     - context_switches_initial;

    gettimeofday(&TV, NULL);

    elapsed_time = (double) TV.tv_sec + 1.0e-6 * ((double) TV.tv_usec)
                 - elapsed_time_initial;

}



/*===========================================================*/
/* routine to reverse the byte order in each event record    */
/*===========================================================*/
void reverse_byte_order(char * in, char * out, int num_bytes)
{
   int i;

   for (i=0; i<num_bytes; i+=sizeof(struct eventstruct))
   {
       swap8(&in[i],    &out[i]);     /* 8-bytes for tbeg                */
       swap8(&in[i+8],  &out[i+8]);   /* 8-bytes for tend                */
       swap4(&in[i+16], &out[i+16]);  /* 4-bytes for taskid              */
       swap4(&in[i+20], &out[i+20]);  /* 4-bytes for eventid             */
       swap4(&in[i+24], &out[i+24]);  /* 4-bytes for src                 */
       swap4(&in[i+28], &out[i+28]);  /* 4-bytes for dest                */
       swap4(&in[i+32], &out[i+32]);  /* 4-bytes for bytes               */
       swap4(&in[i+36], &out[i+36]);  /* 4-bytes for parent address      */
       swap4(&in[i+40], &out[i+40]);  /* 4-bytes for grandparent address */
       swap4(&in[i+44], &out[i+44]);  /* 4-bytes for abi                 */
   }
}

void swap8(char * in, char * out)
{
   out[7] = in[0];
   out[6] = in[1];
   out[5] = in[2];
   out[4] = in[3];
   out[3] = in[4];
   out[2] = in[5];
   out[1] = in[6];
   out[0] = in[7];
}

void swap4(char * in, char * out)
{
   out[3] = in[0];
   out[2] = in[1];
   out[1] = in[2];
   out[0] = in[3];
}


/*===========================================================*/
/* routine to write a trace file with optional byte-swap     */
/*===========================================================*/
void write_tracefile(FILE * fd, struct eventstruct * ev, int nbytes)
{
   int rc, chunk, nchunks, leftover;
   int chunksize;
   char swapped[32768]; /* must be adequate for 400 records */
   char * buffer;


   if (swap_bytes)
   {
     chunksize = 400*sizeof(struct eventstruct);
     nchunks = nbytes/chunksize;
     leftover = nbytes - nchunks*chunksize;

     buffer = (char *) ev;
     for (chunk=0; chunk<nchunks; chunk++)
     {
        reverse_byte_order(buffer, swapped, chunksize);
        rc = fwrite(swapped, 1, chunksize, fd);
        if (rc != chunksize) perror("write_tracefile");
        buffer += chunksize;
     }

     if (leftover != 0)
     {
        reverse_byte_order(buffer, swapped, leftover);
        rc = fwrite(swapped, 1, leftover, fd);
        if (rc != leftover) perror("write_tracefile");
     }
   }

   else
   {
     rc = fwrite(ev, 1, nbytes, fd);
     if (rc != nbytes) perror("write_tracefile");
   }
}



#include <stdlib.h>
#include <sys/debug.h>

#ifndef FRAME_LAYOUT
#define FRAME_LAYOUT
 
struct frame;
 
struct links
{
   struct frame * sp;
   long cr;
   void * lr;
   void * buff[2];
   int * toc;
};
 
struct frame
{
   struct links linkarea;
   int parms[8];
};
 
struct frame * fpThis(int);
struct frame * fpPrev(struct frame *);
 
#endif

/*----------------------------------------------*/
/* function to return the current frame pointer */
/*----------------------------------------------*/
struct frame *fpThis(int i)
{
   return (struct frame *)((char *)&i - sizeof(struct links));
}
 
 
/*-----------------------------------------------------------*/
/* function to return the MPI parent and grandparent address */
/*-----------------------------------------------------------*/
void get_parent(int level, int * parent, int * grandparent)
{
   int tblevel;
   struct frame * fp;

  *parent      = F_ADDRESS;
  *grandparent = F_ADDRESS;

   fp = fpThis(0)->linkarea.sp;

   for (tblevel=0; tblevel<level+2; tblevel++) fp = fp->linkarea.sp;

   if (fp != NULL) *parent = (int) fp->linkarea.lr - sizeof(long);

   fp = fp->linkarea.sp;

   if (fp != NULL) *grandparent = (int) fp->linkarea.lr - sizeof(long);

   return;
}


/*=============================================================*/
/* routine to take an instruction address and return its index */
/*=============================================================*/
int index_from_address(int address)
{
   int i, k, match;

   match = 0;

   /*----------------------------*/
   /* first check the fifo queue */
   /*----------------------------*/
   if (profile_block > FIFO_DEPTH)
   {
      for (k=0; k<FIFO_DEPTH; k++)
      {
          i = profile_fifo[k];
          if (i >= 0)
          {
              if (address == profile_callsite[i])
              {
                  match = 1;
                  break;
              }
          }
      }
   }

   if (match == 1) return i;

   /*-------------------------------------------------------*/
   /* not found in the fifo, so check all known code blocks */
   /*-------------------------------------------------------*/
   for (i=0; i<profile_block; i++)
   {
       if (address == profile_callsite[i])
       {
           match = 1;
           break;
       }
   }

   /*------------------------------------------------*/
   /* if there is no match, this is a new code block */
   /*------------------------------------------------*/
   if (match == 0)
   {
       i = profile_block;
       profile_callsite[i] = address;
       profile_block ++;
   }

   /*-----------------------------------------------*/
   /* save the latest code block in the fifo        */
   /*-----------------------------------------------*/
   for (k=FIFO_DEPTH-2; k>=0; k--) profile_fifo[k+1] = profile_fifo[k];

   profile_fifo[0] = i;

   return i;

}


/*==========================================================*/
/* routine to print the profile with instruction addresses  */
/*==========================================================*/
void print_profile_by_call_site(FILE * fh)
{
    int i, k, id;
    int * profile_sorted_index;

    profile_sorted_index = (int *) malloc(profile_block*sizeof(int));

    mpitrace_sortx(profile_elapsed_time, profile_block, profile_sorted_index, SORT_DESCENDING_ORDER);

    fprintf(fh, "\n");
    fprintf(fh,"-----------------------------------------------------------------\n");
    fprintf(fh, "Profile by call site, traceback level = %d\n", traceback_level);
    fprintf(fh,"-----------------------------------------------------------------\n");
    fprintf(fh, "Use addr2line to map the address to source file and line number.\n");
    fprintf(fh, "Ensure -g is used along with the linker option -bnoobjreorder.\n");
    fprintf(fh,"-----------------------------------------------------------------\n");
    for (i=0; i<profile_block; i++)
    {
        k = profile_sorted_index[i];
        if (profile_elapsed_time[i] > 1.0e-3)
        {
#ifdef __64BIT__
           fprintf(fh, " \ncommunication time = %.3f sec, call site address = %#11.9x\n", 
                   profile_elapsed_time[i], ADDRESS_OFFSET_64BIT + (long) profile_callsite[k]);
#else
           fprintf(fh, " \ncommunication time = %.3f sec, call site address = %#10.8x\n", 
                   profile_elapsed_time[i], profile_callsite[k]);
#endif
           fprintf(fh, "   MPI Routine                  #calls        time(sec)\n");
           for (id=0; id<MAX_IDS; id++)
           {
               if (profile_call_count[k][id] > 0LL)
               {
                   fprintf(fh, "   %-22s %12lld    %12.3f\n", 
                           label[id], profile_call_count[k][id], profile_callsite_time[k][id]);
               }
           }
        }
    }
    fprintf(fh, "\n");

    return;
}
