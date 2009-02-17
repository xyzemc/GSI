
   rc = PMPI_Comm_rank(MPI_COMM_WORLD, &taskid);

   rc = PMPI_Comm_size(MPI_COMM_WORLD, &ntasks);

   rc = PMPI_Comm_group(MPI_COMM_WORLD, &world_group);

   get_node_info(&node_info);

   ncpus          = node_info.ncpus;
   tasks_in_node  = node_info.tasks_per_node;
   rank_in_node   = node_info.rank_in_node;
   all_numthreads = node_info.all_numthreads;

   /*----------------------------------------------------*/
   /* get env variables that control binding and tracing */
   /*----------------------------------------------------*/

   ptr = getenv("TRACE_ALL_EVENTS");
   if (ptr == NULL) trace_events = 0;
   else if (strncasecmp(ptr,"yes",3) == 0)
   {
        trace_events = 1;
   }
   else trace_events = 0;

   ptr = getenv("TRACE_BUFFER_SIZE");
   if (ptr != NULL)
   {
      max_events = atoi(ptr) / sizeof(struct eventstruct);
   }
   else max_events = 50000;

   event = (struct eventstruct *) malloc(max_events*sizeof(struct eventstruct));
   if (event == NULL)
   {
      if (taskid == 0) 
          printf("malloc failed for the events array, tracing is disabled\n");
      trace_events = 0;
   }

   smt_packed = 0;
   ptr = getenv("SMT_PACKED");
   if (ptr != NULL)
   {
      if (strncasecmp(ptr,"yes",3) == 0) smt_packed = 1;
   }

   bind_flag = 0;
   ptr = getenv("BIND_TASKS");
   if (ptr != NULL)
   {
      if (strncasecmp(ptr,"yes",3) == 0)
      {
         /*-----------------------------------------------------------*/
         /* for OpenMP + MPI, pack threads for each process onto cpus */
         /*-----------------------------------------------------------*/
         total_threads = 0;
         for(i=0; i<tasks_in_node; i++) total_threads += all_numthreads[i];

         if (  __SMT_ENABLED() && (total_threads <= ncpus/2) ) cpu_inc = 2;
         else                                                  cpu_inc = 1;

         if (smt_packed) cpu_inc = 1;

         ptr  = getenv("BIND_INC");

         if (ptr != NULL) 
         {
             cpu_inc = atoi(ptr);
             if (cpu_inc < 1) cpu_inc = 1;
         }

         ptr = getenv("BIND_BASE");

         if (ptr != NULL) cpu_base = atoi(ptr);
         else             cpu_base = 0;

         my_base = cpu_base;
         for (i=0; i<rank_in_node; i++) my_base += cpu_inc*all_numthreads[i];

#ifdef _OPENMP
#pragma OMP parallel private(thread_id, my_cpu, rc)
{
         thread_id = omp_get_thread_num();

         my_cpu = (my_base + cpu_inc*thread_id)%ncpus;

         rc = bindprocessor(BINDTHREAD, (int) thread_self(), (cpu_t) my_cpu);

         bind_flag = 1;

         if (rc == 0) fprintf(stderr, "task %d thread %d is bound to cpu %d\n",     taskid, thread_id, my_cpu);
         else         fprintf(stderr, "task %d thread %d did not bind to cpu %d\n", taskid, thread_id, my_cpu);
}
#else
         /*------------------------------------------------------*/
         /* for pure MPI, spread tasks or use BIND_INC/BIND_BASE */
         /*------------------------------------------------------*/
         ptr  = getenv("BIND_INC");

         if (ptr != NULL) 
         {
             cpu_inc = atoi(ptr);
             if (cpu_inc < 1) cpu_inc = 1;
         }

         ptr = getenv("BIND_BASE");

         if (ptr != NULL) cpu_base = atoi(ptr);
         else             cpu_base = 0;

         my_base = cpu_base + cpu_inc*rank_in_node;

         my_cpu = my_base % ncpus;

         rc = bindprocessor(BINDTHREAD, (int) thread_self(), (cpu_t) my_cpu);

         bind_flag = 1;

         if (rc == 0) fprintf(stderr, "task %d is bound to cpu %d\n", taskid, my_cpu);
         else         fprintf(stderr, "task %d did not bind to cpu %d\n", taskid, my_cpu);
#endif

      }
   }

   ptr = getenv("SWAP_BYTES");
   if (ptr == NULL) swap_bytes = 1;
   else if (strncasecmp(ptr,"yes",3) == 0)
   {
        swap_bytes = 1;
   }
   else swap_bytes = 0;

   ptr = getenv("TRACEBACK_LEVEL");
   if (ptr == NULL) traceback_level = 0;
   else             traceback_level = atoi(ptr);

   /*-------------------------------------*/
   /* for now, save all tasks by default  */
   /*-------------------------------------*/
   ptr = getenv("SAVE_ALL_TASKS");
   if (ptr != NULL)
   {
      if (strncasecmp(ptr,"no",2) == 0) save_all_tasks = 0;
   }

   /*---------------------------------------*/
   /* limit event tracing to taskid < 256   */
   /* unless TRACE_ALL_TASKS = yes          */
   /* or if  TRACE_MAX_RANK = #tasks is set */
   /*---------------------------------------*/
   trace_max_rank = (ntasks < 256) ? ntasks : 256;
   ptr = getenv("TRACE_ALL_TASKS");
   if (ptr != NULL)
   {
      if (strncasecmp(ptr,"yes",3) == 0) trace_max_rank = ntasks;
   }

   ptr = getenv("TRACE_MAX_RANK");
   if (ptr != NULL) trace_max_rank = atoi(ptr);
   if (trace_max_rank > ntasks) trace_max_rank = ntasks;

   ptr = getenv("PROFILE_BY_CALL_SITE");
   if (ptr == NULL) profile_by_call_site = 0;
   else if (strncasecmp(ptr,"yes",3) == 0)
   {
        profile_by_call_site = 1;
   }
   else profile_by_call_site = 0;

   for (i=0; i<MAX_PROFILE_BLOCKS; i++)
   {
       profile_elapsed_time[i] = 0.0;

       for (id=0; id<MAX_IDS; id++)
       {
           profile_call_count[i][id] = 0LL;
           profile_callsite_time[i][id] = 0.0;
       }
   }


   ptr = getenv("PROFILE_BY_SUBROUTINE");
   if (ptr == NULL) call_graph = 0;
   else if (strncasecmp(ptr,"yes",3) == 0)
   {
        call_graph = 1;
        callgraph_level = 2 + traceback_level;
   }
   else call_graph = 0;

   for (i=0; i<MAX_CALLERS; i++)
   {
       caller_time[i] = 0.0;

       for (id=0; id<MAX_IDS; id++)
       {
           callgraph_count[i][id] = 0LL;
           callgraph_time[i][id] = 0.0;
       }
   }


   ptr = getenv("PROFILE_BY_NODE");
   if (ptr == NULL) profile_by_node = 0;
   else if (strncasecmp(ptr,"yes",3) == 0)
   {
        profile_by_node = 1;
   }
   else profile_by_node = 0;


   if (profile_by_node)
   {
      for (id=0; id<MAX_IDS; id++)
      {
          on_node_event_count[id] = 0LL;
          on_node_total_time[id] = 0.0;
          on_node_total_bytes[id] = 0.0;

          for(bin=0; bin<MAX_BINS; bin++)
          {
              on_node_bin_count[id][bin] = 0LL;
              on_node_bin_bytes[id][bin] = 0.0;
              on_node_bin_time[id][bin] = 0.0;
          }
      }
   }

   /*-----------------------------------------------------*/
   /* save the initial time after barrier synchronization */
   /*-----------------------------------------------------*/
   rc = PMPI_Barrier(MPI_COMM_WORLD);
   WTIME(TB_INIT);
   time_init = TCONV(TB_INIT);

#ifdef HPM
   if ( __SMT_ENABLED() && (tasks_in_node > ncpus/2) ) smt_packed = 1;

   list_ptr = getenv("HPM_GROUP_LIST");
   if (list_ptr != NULL)
   {
#ifdef _OPENMP
#else
       if (smt_packed) 
       {
           if (!bind_flag)  /* force binding */
           {
               my_cpu = rank_in_node;

               rc = bindprocessor(BINDTHREAD, (int) thread_self(), (cpu_t) my_cpu);

               if (rc == 0) fprintf(stderr,"task %d is bound to cpu %d\n", taskid, my_cpu);
               else         fprintf(stderr,"task %d did not bind to cpu %d\n", taskid, my_cpu);
           }
       }
#endif

       if (strncasecmp(list_ptr, "power5_std", 10) == 0)
       {
          num_groups = 70;
          if (smt_packed) pmgroup = power5_std_list[(taskid/2) % num_groups];
          else            pmgroup = power5_std_list[taskid % num_groups];
       }
       else if (strncasecmp(list_ptr, "power5_plus", 11) == 0)
       {
          num_groups = 79;
          if (smt_packed) pmgroup = power5_plus_list[(taskid/2) % num_groups];
          else            pmgroup = power5_plus_list[taskid % num_groups];
       }
       else
       {
           num_groups = 0;
           ptr = strtok(list_ptr, delimiters);
           while(ptr != NULL)
           {
               pmgroup_list[num_groups] = atoi(ptr);
               ptr = strtok(NULL, delimiters);
               num_groups++;
           }

           if ( (num_groups > ntasks) &&  (taskid == 0) ) 
               fprintf(stderr, "hpm warning: number of groups > ntasks\n");

           if (smt_packed) pmgroup = pmgroup_list[ ((taskid/2) % num_groups) ];
           else            pmgroup = pmgroup_list[ (taskid % num_groups) ];
       }
   }
   else
   {
       ptr = getenv("HPM_GROUP");
       if (ptr == NULL) pmgroup = 0;
       else             pmgroup = atoi(ptr);
   }

   mpihpm_start(pmgroup);
#endif

   getrusage(RUSAGE_SELF, &RU);

   user_time_initial = (double) (RU.ru_utime).tv_sec
             + 1.0e-6*((double) (RU.ru_utime).tv_usec);

   system_time_initial = (double) (RU.ru_stime).tv_sec
               + 1.0e-6*((double) (RU.ru_stime).tv_usec);

   context_switches_initial = RU.ru_nvcsw + RU.ru_nivcsw;

   gettimeofday(&TV, NULL);
   elapsed_time_initial = (double) TV.tv_sec + 1.0e-6 * ((double) TV.tv_usec);

   wrapper_init = 1;
