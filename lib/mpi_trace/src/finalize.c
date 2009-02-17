   char * ptr;
   int rc, id, bin;
   double total_comm, avg_bytes, comm_size;
   struct timeval TV;
   struct rusage RU;
   FILE * fh;
   int myrank, nbytes;
   MPI_Status status;
   FILE * fd;
   int sum_event_number, * event_number_all_tasks;
   int tag = 100;
   int green_light = 1;
   int buffer_overflow_count = 0;
   char * trace_dir = NULL;
   char filename[80];
   char sformat[] = "%-22s %12lld    %11.1f   %12.3f\n";
   char dformat[] = "                    %12lld   %11.1f   %12.3f\n";
   int min_comm_task, max_comm_task, med_comm_task;
   double min_total_comm, max_total_comm, med_total_comm;
   int * sorted_rank;
   double * all_total_comm,  * all_elapsed_time, * all_user_time;
   double * sorted_total_comm;
   long * all_memsizes;
   int * all_context_switches;
   int i, k, output_filter;
   int sorted_index[MAX_CALLERS];
   char cgformat[] ="   %-22s %12lld    %12.3f\n";

   PMPI_Comm_rank(MPI_COMM_WORLD, &myrank);

   if (!wrapper_init)
   {
      fprintf(stderr, "Error from mpitrace: library not initialized for MPI rank %d.\n", myrank);
      PMPI_Finalize();
   }

   PMPI_Barrier(MPI_COMM_WORLD);

   if (collect_summary)
   {
      getrusage(RUSAGE_SELF, &RU);

      user_time = (double) (RU.ru_utime).tv_sec
                + 1.0e-6*((double) (RU.ru_utime).tv_usec)
                - user_time_initial;

      system_time = (double) (RU.ru_stime).tv_sec
                  + 1.0e-6*((double) (RU.ru_stime).tv_usec)
                  - system_time_initial;

      max_memory = RU.ru_maxrss;

      context_switches = RU.ru_nvcsw + RU.ru_nivcsw;

      gettimeofday(&TV, NULL);

      elapsed_time = (double) TV.tv_sec + 1.0e-6 * ((double) TV.tv_usec)
                   - elapsed_time_initial;
   }

#ifdef HPM
     mpihpm_stop();
#endif

   trace_dir = getenv("TRACE_DIR");

   if (trace_dir == NULL)
   {
#ifdef HPM
      if (save_all_tasks) sprintf(filename, "mpi_profile_group%d.%d", pmgroup, myrank);
      else                sprintf(filename, "mpi_profile.%d", myrank);
#else
      sprintf(filename, "mpi_profile.%d", myrank);
#endif
   }
   else
   {
#ifdef HPM
      if (save_all_tasks) sprintf(filename, "%s/mpi_profile_group%d.%d", trace_dir, pmgroup, myrank);
      else                sprintf(filename, "%s/mpi_profile.%d", trace_dir, myrank);
#else
      sprintf(filename, "%s/mpi_profile.%d", trace_dir, myrank);
#endif
   }

   /*----------------------------------------------------*/
   /* compute the total communication time for all tasks */
   /*----------------------------------------------------*/
   total_comm = 0.0;
   for (id=0; id<MAX_IDS; id++)
   {
     if (event_count[id] > 0LL)
     {
       total_comm += total_time[id];
     }
   }

   sorted_rank = (int *) malloc(ntasks*sizeof(int));
   sorted_total_comm = (double *) malloc(ntasks*sizeof(double));
   all_total_comm = (double *) malloc(ntasks*sizeof(double));

   all_elapsed_time = (double *) malloc(ntasks*sizeof(double));
   all_user_time = (double *) malloc(ntasks*sizeof(double));
   all_memsizes = (long *) malloc(ntasks*sizeof(long));
   all_context_switches = (int *) malloc(ntasks*sizeof(int));

   PMPI_Gather(&elapsed_time, 1, MPI_DOUBLE,
               all_elapsed_time, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

   PMPI_Gather(&user_time, 1, MPI_DOUBLE,
               all_user_time, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

   PMPI_Gather(&max_memory, 1, MPI_LONG,
               all_memsizes, 1, MPI_LONG, 0, MPI_COMM_WORLD);

   PMPI_Gather(&context_switches, 1, MPI_INT,
               all_context_switches, 1, MPI_INT, 0, MPI_COMM_WORLD);

   PMPI_Allgather(&total_comm, 1, MPI_DOUBLE,
                     all_total_comm, 1, MPI_DOUBLE, MPI_COMM_WORLD);

   for (i=0; i<ntasks; i++) sorted_total_comm[i] = all_total_comm[i];

   mpitrace_sortx(sorted_total_comm, ntasks, sorted_rank, SORT_ASCENDING_ORDER);

   min_comm_task = sorted_rank[0];
   max_comm_task = sorted_rank[ntasks-1];
   med_comm_task = sorted_rank[(ntasks-1)/2];

   min_total_comm = sorted_total_comm[0];
   max_total_comm = sorted_total_comm[ntasks-1];
   med_total_comm = sorted_total_comm[(ntasks-1)/2];

   if (save_all_tasks) output_filter = 1;
   else
   {
      if (myrank==0 || myrank==min_comm_task || myrank==max_comm_task || myrank==med_comm_task) output_filter = 1;
      else output_filter = 0;
   }

   /*--------------------------------------------*/
   /* write a text output file with summary data */
   /*--------------------------------------------*/
   if (output_filter)
   {
      fh = fopen(filename, "w");
      if (fh == NULL)
      {
         printf("task %d failed to open the trace file %s\n", taskid, filename);
         PMPI_Finalize();
         return;
      }

      total_comm = 0.0;
      fprintf(fh,"Data for MPI rank %d of %d:\n", myrank, ntasks);
      if (collect_summary) fprintf(fh,"Times and statistics from MPI_Init() to MPI_Finalize().\n");
      else                 fprintf(fh,"Times and statistics from summary_start() to summary_stop().\n");
      fprintf(fh, "-----------------------------------------------------------------\n");
      fprintf(fh, "MPI Routine                  #calls     avg. bytes      time(sec)\n");
      fprintf(fh, "-----------------------------------------------------------------\n");
      for (id=0; id<MAX_IDS; id++)
      {
          if (event_count[id] > 0LL)
          {
             total_comm += total_time[id];
             avg_bytes = total_bytes[id] / ((double) event_count[id]);
             fprintf(fh, sformat, label[id], event_count[id], avg_bytes, total_time[id]);
          }
      }
      fprintf(fh, "-----------------------------------------------------------------\n");
      if (myrank == min_comm_task)
         fprintf(fh, "MPI task %d of %d had the minimum communication time.\n", myrank, ntasks);
      if (myrank == max_comm_task)
         fprintf(fh, "MPI task %d of %d had the maximum communication time.\n", myrank, ntasks);
      if (myrank == med_comm_task)
         fprintf(fh, "MPI task %d of %d had the median communication time.\n", myrank, ntasks);
      fprintf(fh, "total communication time = %.3f seconds.\n", total_comm);
      fprintf(fh, "total elapsed time       = %.3f seconds.\n", elapsed_time);
      fprintf(fh, "user cpu time            = %.3f seconds.\n", user_time);
      fprintf(fh, "system time              = %.3f seconds.\n", system_time);
      fprintf(fh, "maximum memory size      = %ld KBytes.\n", max_memory);
      fprintf(fh,"\n");
      fprintf(fh, "-----------------------------------------------------------------\n");
      fprintf(fh,"Message size distributions:\n\n");
      for (id=0; id<MAX_IDS; id++)
      {
          if ( event_count[id] > 0LL  &&  total_bytes[id] > 0.0 )
          {
              fprintf(fh, "%-22s    #calls    avg. bytes      time(sec)\n", label[id]);
              for (bin=0; bin<MAX_BINS; bin++)
              {
                  if (bin_count[id][bin] > 0LL)
                  {
                     comm_size = bin_bytes[id][bin] / ((double) bin_count[id][bin] );
                     fprintf(fh, dformat, bin_count[id][bin], comm_size, bin_time[id][bin]);
                  }
              }
              fprintf(fh, "\n");
          }
      }

      if (profile_by_node)
      {
         total_comm = 0.0;
         fprintf(fh,"\n");
         fprintf(fh, "-----------------------------------------------------------------\n");
         fprintf(fh,"\n\n");
         fprintf(fh, "Summary for on-node communication\n");
         fprintf(fh, "-----------------------------------------------------------------\n");
         fprintf(fh, "MPI Routine                  #calls     avg. bytes      time(sec)\n");
         fprintf(fh, "-----------------------------------------------------------------\n");
         for (id=0; id<MAX_IDS; id++)
         {
             if (on_node_event_count[id] > 0LL)
             {
                total_comm += on_node_total_time[id];
                avg_bytes = on_node_total_bytes[id] / ((double) on_node_event_count[id]);
                fprintf(fh, sformat, label[id], on_node_event_count[id], avg_bytes, on_node_total_time[id]);
             }
         }
         fprintf(fh, "-----------------------------------------------------------------\n");
         fprintf(fh, "measured on-node communication time = %.3f seconds.\n", total_comm);
         fprintf(fh,"\n");
         fprintf(fh, "-----------------------------------------------------------------\n");
         fprintf(fh,"On-node message size distributions:\n\n");
         for (id=0; id<MAX_IDS; id++)
         {
             if ( on_node_event_count[id] > 0LL  &&  on_node_total_bytes[id] > 0.0 )
             {
                 fprintf(fh, "%-22s    #calls    avg. bytes      time(sec)\n", label[id]);
                 for (bin=0; bin<MAX_BINS; bin++)
                 {
                     if (on_node_bin_count[id][bin] > 0LL)
                     {
                        comm_size = on_node_bin_bytes[id][bin] / ((double) on_node_bin_count[id][bin] );
                        fprintf(fh, dformat, on_node_bin_count[id][bin], comm_size, on_node_bin_time[id][bin]);
                     }
                 }
                 fprintf(fh, "\n");
             }
         }
      }

      if (call_graph)
      {
          mpitrace_sortx(caller_time, num_callers, sorted_index, SORT_DESCENDING_ORDER);
          fprintf(fh, "\n");
          fprintf(fh, "-----------------------------------------------------------------\n");
          fprintf(fh, "Call Graph Section:\n");
          for (i=0; i<num_callers; i++)
          {
              k = sorted_index[i];
              if (caller_time[i] > 1.0e-3)
              {
                 fprintf(fh, " \ncommunication time = %.3f sec, parent = %s\n", caller_time[i], callgraph_parent[k]);
                 fprintf(fh, "   MPI Routine                  #calls        time(sec)\n");
                 for (id=0; id<MAX_IDS; id++)
                 {
                     if (callgraph_count[k][id] > 0LL)
                     {
                         fprintf(fh, cgformat, label[id], callgraph_count[k][id], callgraph_time[k][id]);
                     }
                 }
              }
          }
      }
   

      if (profile_by_call_site) print_profile_by_call_site(fh);

      if (taskid == 0)
      {
          fprintf(fh, "\n\n");
          fprintf(fh,"-----------------------------------------------------------------\n");
          fprintf(fh, "\nCommunication summary for all tasks:\n");
          fprintf(fh, "\n");
          fprintf(fh, "  minimum communication time = %.3lf sec for task %d\n",
                  min_total_comm, min_comm_task);
          fprintf(fh, "  median  communication time = %.3lf sec for task %d\n",
                  med_total_comm, med_comm_task);
          fprintf(fh, "  maximum communication time = %.3lf sec for task %d\n",
                  max_total_comm, max_comm_task);
          fprintf(fh, "\n\n");
          fprintf(fh, "MPI tasks sorted by communication time:\n");
          fprintf(fh, "taskid      comm(s)  elapsed(s)     user(s)     size(KB)   switches\n");
          for (i=0; i<ntasks; i++)
          {
             k = sorted_rank[i];
             fprintf(fh, "%6d  %10.2lf  %10.2lf  %10.2lf  %11ld  %10d\n", k,
                           all_total_comm[k], all_elapsed_time[k], all_user_time[k],
                           all_memsizes[k], all_context_switches[k]);
          }
      }

#ifdef HPM
      if (save_all_tasks) mpihpm_print_old_format(fh);
#endif

      fclose(fh);
   }

#ifdef HPM
   if (!save_all_tasks) 
       mpihpm_print(elapsed_time, min_total_comm, max_total_comm, med_total_comm, smt_packed);
#endif

   /*---------------------------------------------------*/
   /* save a binary trace file with data from all tasks */
   /*---------------------------------------------------*/
   PMPI_Allreduce(&event_number, &sum_event_number, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

   if (sum_event_number > 0)
   {
     event_number_all_tasks = (int *) malloc(ntasks*sizeof(int));

     if (myrank == 0) fd = fopen("events.trc", "wb");

     PMPI_Gather(&event_number,1, MPI_INT,
                 event_number_all_tasks, 1, MPI_INT, 0, MPI_COMM_WORLD);

     PMPI_Reduce(&event_buffer_overflow, &buffer_overflow_count, 1, MPI_INT,
                 MPI_SUM, 0, MPI_COMM_WORLD);

     if (myrank == 0)
     {
        nbytes = event_number * sizeof(struct eventstruct);
        if (fd >= 0) write_tracefile(fd, event, nbytes);
        for (id = 1; id<trace_max_rank; id++)
        {
           PMPI_Send(&green_light, 1, MPI_INT, id, tag, MPI_COMM_WORLD);
           nbytes = event_number_all_tasks[id] * sizeof(struct eventstruct);
           PMPI_Recv(event, nbytes, MPI_BYTE, id, tag, MPI_COMM_WORLD, &status);
           if (fd >= 0) write_tracefile(fd, event, nbytes);
        }
        fclose(fd);
        printf("wrote trace file: events.trc\n");
        if (buffer_overflow_count > 0)
            printf("event buffer overflow occured on %d tasks.\n", buffer_overflow_count);
     }
     else if (myrank < trace_max_rank)
     {
        PMPI_Recv(&green_light, 1, MPI_INT, 0, tag, MPI_COMM_WORLD, &status);
        nbytes = event_number * sizeof(struct eventstruct);
        PMPI_Send(event, nbytes, MPI_BYTE, 0, tag, MPI_COMM_WORLD);
     }
   }

