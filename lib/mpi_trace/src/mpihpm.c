/*---------------------------------------------------------*/
/* Simple interface to the powerpc performance counters.   */
/*---------------------------------------------------------*/
/* link with libmpihpm.a -lpmapi -lcfg -lodm               */
/*---------------------------------------------------------*/
/* export HPM_GROUP=[0-152] select group - see power5+.ref */
/*---------------------------------------------------------*/
/*  mpihpm_start(group);  initialize and start counting    */
/*  mpihpm_stop();   stop counting                         */
/*  mpihpm_print(elapsed,...); prints counts               */
/*---------------------------------------------------------*/
/* jhr Modified to be platform independent - the same      */
/* library can be used on all powerpc systems where pmapi  */
/* is supported.                                           */
/*---------------------------------------------------------*/
/* requires aix 5.3 or higher for cpus up to power6.       */
/*---------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pmapi.h>
#include <mpi.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/systemcfg.h>

/*--------------------------------------------------*/
/* added these to get the system model from the odm */
/*--------------------------------------------------*/
#include <odmi.h>
#include <cf.h>
#include <sys/cfgodm.h>
#include <sys/cfgdb.h>

#define MPIHPM_MAX_GROUPS 240

void mpihpm_start(int);
void mpihpm_stop(void);
void mpihpm_print(double, double, double, double, int);
void mpihpm_print_old_format(FILE *);
void get_cpu_type(char *);
void set_gen_labels(int);

/* jhr added lib global vars */ 
static int sys_num_groups, sys_num_events; 
static   pm_info2_t pminfo;
static   pm_groups_info_t pmgroupsinfo;

/* jhr MAX_COUNTERS defined in pmapi.h */ 
static char label[MAX_COUNTERS][MPIHPM_MAX_GROUPS];
static char group_label[MPIHPM_MAX_GROUPS];

static int pmgroup;

pm_prog_t pmprog;

/*=====================================================*/
/* Initialize and start counting in user/process mode. */
/*=====================================================*/
void mpihpm_start(int group)
{
   int rc;
/* jhr moved to static definition 
   pm_info2_t pminfo;
   pm_groups_info_t pmgroupsinfo;
*/
   /*---------------------------------*/
   /* get the group from the argument */
   /*---------------------------------*/
   pmgroup = group;

   /*------------------------------------*/
   /* initialize the performance monitor */
   /*------------------------------------*/
   /* jhr - added PM_GET_GROUPS to get group info */
   rc = pm_initialize(PM_VERIFIED | PM_UNVERIFIED | PM_CAVEAT | PM_GET_GROUPS, 
                      &pminfo, &pmgroupsinfo, PM_CURRENT);
   if (rc != 0) pm_error("mpihpm_start:pm_initialize", rc);
  
   /* jhr Get max number of groups and events/pmcs per group */ 
   sys_num_groups = pmgroupsinfo.maxgroups;
   sys_num_events = pminfo.maxpmcs;

   if (pmgroup == -1) return;

   pmprog.events[0] = pmgroup;
   
   /*-------------------------------------------------------------*/
   /* set the mode for user (not kernel) and process (not thread) */
   /*-------------------------------------------------------------*/
   pmprog.mode.w = 0;
   pmprog.mode.b.user = 1;
   pmprog.mode.b.process = 1;
   pmprog.mode.b.runlatch = 1;

   /*------------------------------------------*/
   /* for power-5 you have to use event groups */
   /*------------------------------------------*/
   pmprog.mode.b.is_group = 1;

   /*--------------------------------------------*/
   /* set the mode to start counting immediately */
   /*--------------------------------------------*/
   pmprog.mode.b.count = 1;

   /*-----------------------------------------*/
   /* initialize the group and start counting */
   /*-----------------------------------------*/
   rc = pm_set_program_mygroup(&pmprog); 
   if (rc != 0) pm_error("mpihpm_start:pm_set_program_mygroup", rc);

   return;
}



/*===============*/
/* stop counting */
/*===============*/
void mpihpm_stop(void)
{
   int rc;
   if (pmgroup == -1) return;
   rc = pm_stop_mygroup();
   if (rc != 0) pm_error("mpihpm_stop:pm_stop_mygroup", rc);
   return;
}



/*============================================*/
/* print the counter values with event labels */
/*============================================*/
void mpihpm_print(double elapsed, double min_comm, 
                   double max_comm, double med_comm, int smt_packed)
{
   int rc, i, k, group, instances, num_groups;
   pm_data_t pmdata;
   int myrank, numtasks, task, ncpus;
   /* jhr - changed NUM_COUNTERS to MAX_COUNTERS  */ 
   long long counter_values[MAX_COUNTERS];
   char host[80];
   FILE * fh;
   time_t current_time;
   double freq;
   int * all_groups;
   int * rank_list;
   long long * all_counter_values;
   struct stat file_status;
   char MyCpuType[16];
   int howmany = 0;
   int getall = 0;
   struct CuAt * Attr;

/* jhr */
   get_cpu_type (MyCpuType);


   if (pmgroup == -1)
   {
       for (i=0; i<sys_num_events; i++) counter_values[i] = 0LL;
   }
   else
   {
       /*--------------------------------------------------------*/
       /* the counters are already stopped, so just get the data */
       /*--------------------------------------------------------*/
       rc = pm_get_program_mygroup(&pmprog);
       if (rc != 0) pm_error("mpihpm_print:pm_get_program_mygroup", rc);

       rc = pm_get_data_mygroup(&pmdata); 
       if (rc != 0) pm_error("mpihpm_print:pm_get_data_mygroup", rc);

       for (i=0; i<sys_num_events; i++) counter_values[i] = pmdata.accu[i];
   }

   PMPI_Comm_rank(MPI_COMM_WORLD, &myrank);
   PMPI_Comm_size(MPI_COMM_WORLD, &numtasks);

   /*-------------------------------*/
   /* Gather counter data to rank 0 */
   /*-------------------------------*/

   all_groups = (int *) malloc(numtasks*sizeof(int));
   all_counter_values = (long long *) malloc(numtasks*sys_num_events*sizeof(long long));

   PMPI_Gather(&pmgroup, 1, MPI_INTEGER, all_groups, 1, MPI_INTEGER, 0, MPI_COMM_WORLD);

   PMPI_Gather(counter_values,     sys_num_events, MPI_LONG_LONG_INT, 
               all_counter_values, sys_num_events, MPI_LONG_LONG_INT, 0, MPI_COMM_WORLD);

   if (myrank > 0) return;
  
   /*------------------------------------*/
   /* MPI rank 0 does the remaining work */
   /*------------------------------------*/

   gethostname(host, sizeof(host));
   for (i=0; i<sizeof(host); i++)
   {
       if (host[i] == '.')
       {
           host[i] = '\0';
           break;
       }
   }

   /*---------------------------------*/
   /* open hpmdata.txt in append mode */
   /*---------------------------------*/
   fh = fopen("hpmdata.txt", "a");

   if (fh == NULL)
   {
      printf("failed to open hpmdata.txt\n");
      return;
   }

   freq = 1.0e-6*pm_cycles();

   time(&current_time);
   num_groups = sys_num_groups;

   rank_list = (int *) malloc(numtasks*sizeof(int));

   /*---------------------------------------*/
   /* write header information for each job */
   /*---------------------------------------*/

   /*-----------------------------------*/
   /* get the model number from the odm */
   /*-----------------------------------*/
   rc = odm_initialize();

   if (rc == 0)
   {
       Attr = getattr("sys0", "modelname", getall, &howmany);

       if (howmany == 1) fprintf(fh,";system model;%s\n", Attr->value);

       odm_terminate();
   }
   else fprintf(fh,";system model;not_checked\n");

   fprintf(fh,";processor type;%s\n",MyCpuType);

   ncpus = _system_configuration.ncpus;

   if (__SMT_ENABLED())
   {
       fprintf(fh,";number of cpus;%d\n", ncpus/2);
       fprintf(fh,";SMT is enabled\n");
   }
   else
   {
       fprintf(fh,";number of cpus;%d\n", ncpus);
       fprintf(fh,";SMT is disabled\n");
   }

   fprintf(fh,";processor frequency (MHz);%.0lf\n", freq);

   fprintf(fh,";host;%s\n", host);

   fprintf(fh,";MPI tasks;%d\n", numtasks);

   fprintf(fh,";date;%s", ctime(&current_time));

   fprintf(fh,";elapsed time (sec);%.2lf\n", elapsed);

   fprintf(fh,";minimum communication time (sec);%.2lf\n", min_comm);
   fprintf(fh,";maximum communication time (sec);%.2lf\n", max_comm);
   fprintf(fh,";median communication time (sec);%.2lf\n", med_comm);

   fprintf(fh,"\n");

   fprintf(fh,"group;counter;counts\n");


   for (group=0; group<num_groups; group++)
   {
       instances = 0;
       for (task=0; task<numtasks;  task++)
       {
           if (all_groups[task] == group)
           {
               rank_list[instances] = task;
               instances++;
           }
       }

       if (instances > 0)
       {
       /* jhr */ 
           set_gen_labels(group);

           for (k=0; k<sys_num_events; k++)
           {
               fprintf(fh,"%d;%-72s;", group, label[k]);

               if (smt_packed) 
               {
                   for (i=0; i<instances; i++)
                   {
                       task = rank_list[i];
                       fprintf(fh,"%lld;", all_counter_values[task*sys_num_events + k]);
                   }
               }
               else
               {
                   for (i=0; i<instances; i++)
                   {
                       task = rank_list[i];
                       fprintf(fh,"%lld;;", all_counter_values[task*sys_num_events + k]);
                   }
               }

               fprintf(fh,"\n");
           }

           fprintf(fh,"\n");
       }
   }

   fclose(fh);

}


/*============================================*/
/* print the counter values with event labels */
/*============================================*/
void mpihpm_print_old_format(FILE * fh)
{
   int rc, i;
   pm_data_t pmdata;
    /* jhr */ 
   char MyCpuType[16];

   /*--------------------------------------------------------*/
   /* the counters are already stopped, so just get the data */
   /*--------------------------------------------------------*/
   rc = pm_get_program_mygroup(&pmprog);
   if (rc != 0) pm_error("mpihpm_print", rc);

   rc = pm_get_data_mygroup(&pmdata);
   if (rc != 0) pm_error("mpihpm_print", rc);

   /* jhr */ 
   get_cpu_type (MyCpuType);
   set_gen_labels(pmgroup);

   
   fprintf(fh, "\n");
   fprintf(fh, "--------------------------------------------------------------------------\n");
   fprintf(fh, "%s counter report for group %d.\n", MyCpuType, pmgroup);
   fprintf(fh, " %s\n", group_label);
   fprintf(fh, "--------------------------------------------------------------------------\n");
   for (i=0; i<sys_num_events; i++)
   {
        fprintf(fh, "%15lld  %s\n", pmdata.accu[i], label[i]);
   }
   fprintf(fh, "\n");

   return;

}

/*---------------------------------------------------------*/
/* jhr Generic get lables should work on all platforms     */ 
/* provided the PM_GET_GROUPS is ORed into the init filter */  
/*---------------------------------------------------------*/

void set_gen_labels(int group) {
int j, shared;
int counter, event;
pm_events2_t  *evp;

   /* jhr Set group long name or is description required? */
   sprintf(group_label,"%s, %s", pmgroupsinfo.event_groups[group].short_name,
   pmgroupsinfo.event_groups[group].long_name);

   /* get the event id from the list  in the group*/
   for (counter = 0; counter < pminfo.maxpmcs; counter++) {
#ifdef FPMDEBUG
         printf("Counter %2d, ", counter+1);
#endif
         event = pmgroupsinfo.event_groups[group].events[counter];
         if ((event == COUNT_NOTHING) || (pminfo.maxevents[counter] == 0)) {
            j = 0;
#ifdef FPMDEBUG
            printf("event %2d: No event\n", event);
#endif
         }
         else {
           /* find pointer to the event */
            shared = 0;
            for (j = 0; j < pminfo.maxevents[counter]; j++) {
               evp = pminfo.list_events[counter]+j;
               if (event == evp->event_id) {
                  if (evp->status.b.shared) shared = 1;
                  break;
               }
             }

         /* Initialize event description for each counter
            in the descriptor. */

             if (shared)
                 sprintf(label[counter],"%s [shared] (%s)",evp->long_name, evp->short_name);
             else
                 sprintf(label[counter],"%s (%s)",evp->long_name, evp->short_name);
         }
   }

}

void get_cpu_type(char *cputype) { 
 /* Need to determine processor type */  
   
    switch (_system_configuration.version) {
       case PV_4:
           strcpy(cputype, "Power4");
           break;
       case PV_4_2:
           strcpy(cputype, "Power4-II");
           break;
       case PV_4_3:
           strcpy(cputype, "ppc970");
           break;
       case PV_5:
           strcpy(cputype, "Power5");
           break;
       case PV_5_2:
           strcpy(cputype, "Power5+");
           break;
       case PV_5_3:
           strcpy(cputype, "Power5++");
           break;
       case PV_6:
           strcpy(cputype, "Power6");
           break;
#ifdef PV_6_Compat
       case PV_6_Compat:
           strcpy(cputype, "Power6");
           break;
#endif
        default :
           strcpy(cputype, "CPU Type N/A");
           break;
       }
   return;
}
