/*============================================================*/
/* Wrappers for overall timing from MPI_Init to MPI_Finalize .*/
/* Timing data is reported when MPI_Finalize is called.       */
/* Please e-mail corrections/errors to walkup@us.ibm.com.     */
/*============================================================*/
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

/*----------------------------------------------------------*/
/*    elapsed-time timing functions                         */
/*----------------------------------------------------------*/
#define WTIME(TV) gettimeofday(&TV, NULL)
#define TCONV(TV) (double) (TV).tv_sec + 1.0e-6*((double) (TV).tv_usec)

#ifdef EXTNAME
#define mpi_init             mpi_init_
#define mpi_finalize         mpi_finalize_
#endif


/*----------------------------------------------------------*/
/*    variables with file scope                             */
/*----------------------------------------------------------*/
static int taskid, ntasks;
static double time_initial, time_final, tconv, fhigh;
static double user_time;
static double system_time;

/*----------------------------------------------------------*/
/*    function prototypes                                   */
/*----------------------------------------------------------*/
void mpi_init(int *);
void mpi_finalize(int *);

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_init                         */
/*----------------------------------------------------------*/
void mpi_init(int * info)
{
   int id, rc, bin;
   struct timeval TV;
   struct rusage RU;

   rc = PMPI_Init(NULL, NULL);

   PMPI_Comm_rank(MPI_COMM_WORLD, &taskid);
   PMPI_Comm_size(MPI_COMM_WORLD, &ntasks);

   WTIME(TV);
   time_initial = TCONV(TV);

   getrusage(RUSAGE_SELF, &RU);

   user_time = (double) (RU.ru_utime).tv_sec 
             + 1.0e-6*((double) (RU.ru_utime).tv_usec);

   system_time = (double) (RU.ru_stime).tv_sec 
               + 1.0e-6*((double) (RU.ru_stime).tv_usec);

   *info = rc;

   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_finalize                     */
/*----------------------------------------------------------*/
void mpi_finalize(int * info)
{
   int i, rc, id, bin, max_memory;
   int context_switches, *switches;
   double wall_time, total_comm, avg_bytes, comm_size;
   struct timeval TV;
   struct rusage RU;
   double max_elapsed;
   double total_mem;
   double total_user;
   double total_sys;
   int * memsizes;
   double *walltimes;
   double *usertimes;
   double *systimes;
   char host[80];
   char * names, * ptr;

   getrusage(RUSAGE_SELF, &RU);

   user_time = (double) (RU.ru_utime).tv_sec 
             + 1.0e-6*((double) (RU.ru_utime).tv_usec) - user_time;

   system_time = (double) (RU.ru_stime).tv_sec 
               + 1.0e-6*((double) (RU.ru_stime).tv_usec) - system_time;

   max_memory = RU.ru_maxrss;

   WTIME(TV);
   time_final = TCONV(TV);

   wall_time = time_final - time_initial;

   context_switches = RU.ru_nvcsw + RU.ru_nivcsw;

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

   /*-------------------------------------------*/
   /* allocate memory for arrays of system info */
   /*-------------------------------------------*/
   walltimes = (double *) malloc(ntasks*sizeof(double));
   usertimes = (double *) malloc(ntasks*sizeof(double));
   systimes = (double *) malloc(ntasks*sizeof(double));
   memsizes = (int *) malloc(ntasks*sizeof(int));
   switches = (int *) malloc(ntasks*sizeof(int));

   /*----------------------------------------*/
   /* gather the arrays to task 0 for output */
   /*----------------------------------------*/
   MPI_Gather(host, sizeof(host), MPI_BYTE, names, sizeof(host), MPI_BYTE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&wall_time, 1, MPI_DOUBLE, walltimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&user_time, 1, MPI_DOUBLE, usertimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&system_time, 1, MPI_DOUBLE, systimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&max_memory, 1, MPI_INT, memsizes, 1, MPI_INT,
              0, MPI_COMM_WORLD);
   MPI_Gather(&context_switches, 1, MPI_INT, switches, 1, MPI_INT,
              0, MPI_COMM_WORLD);

   if (taskid == 0)
   {
      total_mem = 0.0;
      total_user = 0.0;
      total_sys = 0.0;
      max_elapsed = wall_time;

      printf("\n\nSummary of the utilization of system resources:\n\n");
      printf("task      hostname     wall(s)    user(s)     sys(s)    size(KB)  pswitches\n");
      fflush(stdout);
      for (i=0; i<ntasks; i++)
      {
          if (walltimes[i] > max_elapsed) max_elapsed = walltimes[i];
          total_mem += (double) memsizes[i];
          total_user += usertimes[i];
          total_sys  += systimes[i];
          ptr = names + i*sizeof(host);
          printf("%3d %14s %10.2lf %10.2lf %10.2lf %11d %11d\n",
                 i, ptr, walltimes[i], usertimes[i], systimes[i], memsizes[i], switches[i]);
      }
      printf("\n");
      printf("Maximum elapsed time = %.2f seconds.\n", max_elapsed);
      printf("Aggregate memory utilization = %.2f MBytes.\n", total_mem/1024.0); 
      printf("Aggregate user time   = %.2f seconds.\n", total_user);
      printf("Aggregate system time = %.2f seconds.\n", total_sys);
      printf("\n");

       
      fflush(stdout);
   }

   /*----------------------------*/
   /* free memory for the arrays */
   /*----------------------------*/
   free(walltimes);
   free(usertimes);
   free(systimes);
   free(memsizes);
   free(switches);

   rc = PMPI_Finalize();
  *info = rc;
   return;

}

/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Init                               */
/*----------------------------------------------------------*/
int MPI_Init(int * argc, char *** argv)
{
   int id, rc, bin;
   struct timeval TV;
   struct rusage RU;

   rc = PMPI_Init(argc, argv);

   PMPI_Comm_rank(MPI_COMM_WORLD, &taskid);
   PMPI_Comm_size(MPI_COMM_WORLD, &ntasks);

   WTIME(TV);
   time_initial = TCONV(TV);

   getrusage(RUSAGE_SELF, &RU);

   user_time = (double) (RU.ru_utime).tv_sec
             + 1.0e-6*((double) (RU.ru_utime).tv_usec);

   system_time = (double) (RU.ru_stime).tv_sec
               + 1.0e-6*((double) (RU.ru_stime).tv_usec);


   return rc;
}
/*----------------------------------------------------------*/
/*    wrapper for C: MPI_Finalize                           */
/*----------------------------------------------------------*/
int MPI_Finalize(void)
{
   int i, rc, id, bin, max_memory;
   int context_switches, *switches;
   double wall_time, total_comm, avg_bytes, comm_size;
   struct timeval TV;
   struct rusage RU;
   double max_elapsed;
   double total_mem;
   double total_user;
   double total_sys;
   int * memsizes;
   double *walltimes;
   double *usertimes;
   double *systimes;
   char host[80];
   char * names, * ptr;

   getrusage(RUSAGE_SELF, &RU);

   user_time = (double) (RU.ru_utime).tv_sec 
             + 1.0e-6*((double) (RU.ru_utime).tv_usec) - user_time;

   system_time = (double) (RU.ru_stime).tv_sec 
               + 1.0e-6*((double) (RU.ru_stime).tv_usec) - system_time;

   max_memory = RU.ru_maxrss;

   WTIME(TV);
   time_final = TCONV(TV);

   wall_time = time_final - time_initial;

   context_switches = RU.ru_nvcsw + RU.ru_nivcsw;

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

   /*-------------------------------------------*/
   /* allocate memory for arrays of system info */
   /*-------------------------------------------*/
   walltimes = (double *) malloc(ntasks*sizeof(double));
   usertimes = (double *) malloc(ntasks*sizeof(double));
   systimes = (double *) malloc(ntasks*sizeof(double));
   memsizes = (int *) malloc(ntasks*sizeof(int));
   switches = (int *) malloc(ntasks*sizeof(int));

   /*----------------------------------------*/
   /* gather the arrays to task 0 for output */
   /*----------------------------------------*/
   MPI_Gather(host, sizeof(host), MPI_BYTE, names, sizeof(host), MPI_BYTE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&wall_time, 1, MPI_DOUBLE, walltimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&user_time, 1, MPI_DOUBLE, usertimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&system_time, 1, MPI_DOUBLE, systimes, 1, MPI_DOUBLE,
              0, MPI_COMM_WORLD);
   MPI_Gather(&max_memory, 1, MPI_INT, memsizes, 1, MPI_INT,
              0, MPI_COMM_WORLD);
   MPI_Gather(&context_switches, 1, MPI_INT, switches, 1, MPI_INT,
              0, MPI_COMM_WORLD);

   if (taskid == 0)
   {
      total_mem = 0.0;
      total_user = 0.0;
      total_sys = 0.0;
      max_elapsed = wall_time;

      printf("\n\nSummary of the utilization of system resources:\n\n");
      printf("task      hostname     wall(s)    user(s)     sys(s)    size(KB)  pswitches\n");
      fflush(stdout);
      for (i=0; i<ntasks; i++)
      {
          if (walltimes[i] > max_elapsed) max_elapsed = walltimes[i];
          total_mem += (double) memsizes[i];
          total_user += usertimes[i];
          total_sys  += systimes[i];

          ptr = names + i*sizeof(host);

          printf("%3d %14s %10.2lf %10.2lf %10.2lf %11d %11d\n",
                 i, ptr, walltimes[i], usertimes[i], systimes[i], memsizes[i], switches[i]);
      }
      printf("\n");
      printf("Maximum elapsed time = %.2f seconds.\n", max_elapsed);
      printf("Aggregate memory utilization = %.2f MBytes.\n", total_mem/1024.0); 
      printf("Aggregate user time   = %.2f seconds.\n", total_user);
      printf("Aggregate system time = %.2f seconds.\n", total_sys);
      printf("\n");

       
      fflush(stdout);
   }

   /*----------------------------*/
   /* free memory for the arrays */
   /*----------------------------*/
   free(walltimes);
   free(usertimes);
   free(systimes);
   free(memsizes);
   free(switches);

   rc = PMPI_Finalize();
   return rc;

}
