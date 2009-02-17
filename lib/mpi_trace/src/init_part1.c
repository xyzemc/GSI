   for (id=0; id<MAX_IDS; id++)
   {
       event_count[id] = 0LL;
       total_time[id] = 0.0;
       total_bytes[id] = 0.0;

       for(bin=0; bin<MAX_BINS; bin++) 
       {
           bin_count[id][bin] = 0LL;
           bin_bytes[id][bin] = 0.0;
           bin_time[id][bin] = 0.0;
       }
   }

   if (_system_configuration.implementation == POWER_RS2)
   {
       fhigh = 1.0;
       tconv = 1.0;
   }
   else  /* powerpc family */
   {
       fhigh = 4.294967296;
       tb_top = (double) _system_configuration.Xint;
       tb_bot = (double) _system_configuration.Xfrac;
       tconv = tb_top/tb_bot; 
   }

   strcpy(label[COMM_SIZE_ID],        "MPI_Comm_size");
   strcpy(label[COMM_RANK_ID],        "MPI_Comm_rank");
   strcpy(label[SEND_ID],             "MPI_Send");
   strcpy(label[SSEND_ID],            "MPI_Ssend");
   strcpy(label[RSEND_ID],            "MPI_Rsend");
   strcpy(label[BSEND_ID],            "MPI_Bsend");
   strcpy(label[ISEND_ID],            "MPI_Isend");
   strcpy(label[ISSEND_ID],           "MPI_Issend");
   strcpy(label[IRSEND_ID],           "MPI_Irsend");
   strcpy(label[IBSEND_ID],           "MPI_Ibsend");
   strcpy(label[SEND_INIT_ID],        "MPI_Send_init");
   strcpy(label[SSEND_INIT_ID],       "MPI_Ssend_init");
   strcpy(label[RSEND_INIT_ID],       "MPI_Rsend_init");
   strcpy(label[BSEND_INIT_ID],       "MPI_Bsend_init");
   strcpy(label[RECV_INIT_ID],        "MPI_Recv_init");
   strcpy(label[RECV_ID],             "MPI_Recv");
   strcpy(label[IRECV_ID],            "MPI_Irecv");
   strcpy(label[SENDRECV_ID],         "MPI_Sendrecv");
   strcpy(label[SENDRECV_REPLACE_ID], "MPI_Sendrecv_replace");
   strcpy(label[BUFFER_ATTACH_ID],    "MPI_Buffer_attach");
   strcpy(label[BUFFER_DETACH_ID],    "MPI_Buffer_detach");
   strcpy(label[PROBE_ID],            "MPI_Probe");
   strcpy(label[IPROBE_ID],           "MPI_Iprobe");
   strcpy(label[TEST_ID],             "MPI_Test");
   strcpy(label[TESTANY_ID],          "MPI_Testany");
   strcpy(label[TESTALL_ID],          "MPI_Testall");
   strcpy(label[TESTSOME_ID],         "MPI_Testsome");
   strcpy(label[WAIT_ID],             "MPI_Wait");
   strcpy(label[WAITANY_ID],          "MPI_Waitany");
   strcpy(label[WAITALL_ID],          "MPI_Waitall");
   strcpy(label[WAITSOME_ID],         "MPI_Waitsome");
   strcpy(label[START_ID],            "MPI_Start");
   strcpy(label[STARTALL_ID],         "MPI_Startall");
   strcpy(label[BCAST_ID],            "MPI_Bcast");
   strcpy(label[BARRIER_ID],          "MPI_Barrier");
   strcpy(label[REDUCE_ID],           "MPI_Reduce");
   strcpy(label[ALLREDUCE_ID],        "MPI_Allreduce");
   strcpy(label[REDUCE_SCATTER_ID],   "MPI_Reduce_scatter");
   strcpy(label[GATHER_ID],           "MPI_Gather");
   strcpy(label[GATHERV_ID],          "MPI_Gatherv");
   strcpy(label[SCAN_ID],             "MPI_Scan");
   strcpy(label[ALLGATHER_ID],        "MPI_Allgather");
   strcpy(label[ALLGATHERV_ID],       "MPI_Allgatherv");
   strcpy(label[SCATTER_ID],          "MPI_Scatter");
   strcpy(label[SCATTERV_ID],         "MPI_Scatterv");
   strcpy(label[ALLTOALL_ID],         "MPI_Alltoall");
   strcpy(label[ALLTOALLV_ID],        "MPI_Alltoallv");

   strcpy(label[IREDUCE_ID],          "MPE_Ireduce");
   strcpy(label[IBCAST_ID],           "MPE_Ibcast");
   strcpy(label[ISCATTERV_ID],        "MPE_Iscatterv");
   strcpy(label[IALLREDUCE_ID],       "MPE_Iallreduce");
   strcpy(label[IALLTOALLV_ID],       "MPE_Ialltoallv");
