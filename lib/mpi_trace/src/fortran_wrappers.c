/*----------------------------------------------------------*/
/*    fortran entry points                                  */
/*----------------------------------------------------------*/
void mpi_init(int *);
void mpi_init_thread(int *, int *, int *);
void mpi_finalize(int *);
void mpi_comm_rank(int *, int *, int *);
void mpi_comm_size(int *, int *, int *);
void mpi_send(void *, int *, int *, int *, int *, int *, int *);
void mpi_ssend(void *, int *, int *, int *, int *, int *, int *);
void mpi_rsend(void *, int *, int *, int *, int *, int *, int *);
void mpi_bsend(void *, int *, int *, int *, int *, int *, int *);
void mpi_isend(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_issend(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_irsend(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_ibsend(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_send_init(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_ssend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_rsend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_bsend_init(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_recv_init(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_recv(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_irecv(void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_sendrecv(void *, int *, int *, int *, int *, 
                  void *, int *, int *, int *, int *, int *, MPI_Status *, int *);
void mpi_sendrecv_replace(void *, int *, int *, int *, int *, 
                          int *, int *, int *, MPI_Status *, int *);
void mpi_buffer_attach(void *, int *, int *);
void mpi_buffer_detach(void *, int *, int *);
void mpi_probe(int *, int *, int *, MPI_Status *, int *);
void mpi_iprobe(int *, int *, int *, int *, MPI_Status *, int *);
void mpi_test(int *, int *, MPI_Status *, int *);
void mpi_testany(int *, int *, int*, int *, MPI_Status *, int *);
void mpi_testall(int *, int*, int *, MPI_Status *, int *);
void mpi_testsome(int *, int *, int *, int *, MPI_Status *, int *);
void mpi_wait(int *, MPI_Status *, int *);
void mpi_waitany(int *, int *, int *, MPI_Status *, int *);
void mpi_waitall(int *, int *, MPI_Status *, int *);
void mpi_waitsome(int *, int *, int *, int *, MPI_Status *, int *);
void mpi_start(int *, int *);
void mpi_startall(int *, int *, int *);
void mpi_bcast(void *, int *, int *, int *, int *, int *);
void mpe_ibcast(void *, int *, int *, int *, int *, int *, int *);
void mpi_barrier(int *, int *);
void mpi_reduce(void *, void *, int *, int *, int *, int *, int *, int *);
void mpe_ireduce(void *, void *, int *, int *, int *, int *, int *, int *, int *);
void mpi_allreduce(void *, void *, int *, int *, int *, int *, int *);
void mpe_iallreduce(void *, void *, int *, int *, int *, int *, int *, int *);
void mpi_reduce_scatter(void *, void *, int *, int *, int *, int *, int *);
void mpi_gather(void *, int *, int *, void *, int *, int *, int *, int *, int *);
void mpi_gatherv(void *, int *, int *, void *, int *, int *, int *,int *, int *, int *);
void mpi_scan(void *, void *, int *, int *, int *, int *, int *);
void mpi_allgather(void *, int *, int *, void *, int *, int *, int *, int *);
void mpi_allgatherv(void *, int *, int *, void *, int *, int *,int *, int *, int *);
void mpi_scatter(void *, int *, int *, void *, int *, int *, int *, int *, int *);
void mpi_scatterv(void *, int *, int *, int *, void *, int *, int *, int *, int *, int *);
void mpe_iscatterv(void *, int *, int *, int *, void *, int *, int *, int *, int *, int *, int *);
void mpi_alltoall(void *, int *, int *, void *, int *, int *, int *, int *);
void mpi_alltoallv(void *, int *, int *, int *, void *, int *, int *,int *, int *, int *);
void mpe_ialltoallv(void *, int *, int *, int *, void *, int *, int *,int *, int *, int *, int *);

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_init                         */
/*----------------------------------------------------------*/
void mpi_init(int * info)
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
   int cpu_base, cpu_inc, my_cpu, my_base;
   struct node_info node_info;
   int bind_flag;

#include "init_part1.c"

   pmpi_init(&rcinit);

#include "init_part2.c"

   *info = rcinit;

   return;
}


/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_init_thread                  */
/*----------------------------------------------------------*/
void mpi_init_thread(int * required, int * provided, int * info)
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
   int cpu_base, cpu_inc, my_cpu, my_base;
   struct node_info node_info;
   int bind_flag;

#include "init_part1.c"

   pmpi_init_thread(required, provided, &rcinit);

#include "init_part2.c"

   *info = rcinit;

   return;
}


/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_comm_rank                    */
/*----------------------------------------------------------*/
void mpi_comm_rank(int * comm, int * id, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_comm_rank(comm, id, info);
   WTIME(TB2);

   LogEvent(COMM_RANK_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_comm_size                    */
/*----------------------------------------------------------*/
void mpi_comm_size(int * comm, int * numtasks, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_comm_size(comm, numtasks, info);
   WTIME(TB2);

   LogEvent(COMM_SIZE_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}


/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_send                         */
/*----------------------------------------------------------*/
void mpi_send(void * sbuf, int * count, int * type, int * dest, 
              int * tag, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_send(sbuf, count, type, dest, tag, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_ssend                        */
/*----------------------------------------------------------*/
void mpi_ssend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_ssend(sbuf, count, type, dest, tag, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_rsend                        */
/*----------------------------------------------------------*/
void mpi_rsend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_rsend(sbuf, count, type, dest, tag, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(RSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_bsend                        */
/*----------------------------------------------------------*/
void mpi_bsend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_bsend(sbuf, count, type, dest, tag, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(BSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_isend                        */
/*----------------------------------------------------------*/
void mpi_isend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_isend(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(ISEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_issend                       */
/*----------------------------------------------------------*/
void mpi_issend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_issend(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(ISSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_irsend                       */
/*----------------------------------------------------------*/
void mpi_irsend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_irsend(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(IRSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_ibsend                       */
/*----------------------------------------------------------*/
void mpi_ibsend(void * sbuf, int * count, int * type, int * dest, 
               int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_ibsend(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(IBSEND_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_send_init                    */
/*----------------------------------------------------------*/
void mpi_send_init(void * sbuf, int * count, int * type, int * dest, 
                   int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_send_init(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SEND_INIT_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_ssend_init                   */
/*----------------------------------------------------------*/
void mpi_ssend_init(void * sbuf, int * count, int * type, int * dest, 
                    int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_ssend_init(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SSEND_INIT_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_rsend_init                   */
/*----------------------------------------------------------*/
void mpi_rsend_init(void * sbuf, int * count, int * type, int * dest, 
                    int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_rsend_init(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(RSEND_INIT_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_bsend_init                   */
/*----------------------------------------------------------*/
void mpi_bsend_init(void * sbuf, int * count, int * type, int * dest, 
                    int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_bsend_init(sbuf, count, type, dest, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(BSEND_INIT_ID, TB1, TB2, -1, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_recv_init                    */
/*----------------------------------------------------------*/
void mpi_recv_init(void * rbuf, int * count, int * type, int * src, 
                   int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_recv_init(rbuf, count, type, src, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(RECV_INIT_ID, TB1, TB2, *src, -1, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_recv                         */
/*----------------------------------------------------------*/
void mpi_recv(void * rbuf, int * count, int * type, int * src, 
              int * tag, int * comm, int * status, int * info)
{
   int bytes, source, count_received, size, rc;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_recv(rbuf, count, type, src, tag, comm, status, info);
   WTIME(TB2);

   source = status[0];
   pmpi_get_count(status, type, &count_received, &rc);
   PMPI_Type_size(*type, &size);
   bytes = count_received * size;

   LogEvent(RECV_ID, TB1, TB2, source, -1, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_irecv                        */
/*----------------------------------------------------------*/
void mpi_irecv(void * rbuf, int * count, int * type, int * src, 
               int * tag, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_irecv(rbuf, count, type, src, tag, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(IRECV_ID, TB1, TB2, *src, -1, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_sendrecv                     */
/*----------------------------------------------------------*/
void mpi_sendrecv(void * sbuf, int * scount, int * stype, int * dest,   int * stag,
                  void * rbuf, int * rcount, int * rtype, int * src, int * rtag,
                  int * comm, MPI_Status * status, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_sendrecv(sbuf, scount, stype, dest, stag,
                 rbuf, rcount, rtype, src,  rtag,
                 comm, status, info);
   WTIME(TB2);

   PMPI_Type_size(*stype, &bytes);
   bytes = (*scount) * bytes;

   LogEvent(SENDRECV_ID, TB1, TB2, *src, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_sendrecv_replace             */
/*----------------------------------------------------------*/
void mpi_sendrecv_replace(void * buf, int * count, int * type, int * dest, int * stag,
                          int * src, int * rtag, int * comm, MPI_Status * status, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_sendrecv_replace(buf, count, type, dest, stag,
                         src, rtag, comm, status, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SENDRECV_REPLACE_ID, TB1, TB2, *src, *dest, bytes, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_buffer_attach                */
/*----------------------------------------------------------*/
void mpi_buffer_attach(void * buffer, int * size, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_buffer_attach(buffer, size, info);
   WTIME(TB2);

   LogEvent(BUFFER_ATTACH_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_buffer_detach                */
/*----------------------------------------------------------*/
void mpi_buffer_detach(void * buffer, int * size, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_buffer_detach(buffer, size, info);
   WTIME(TB2);

   LogEvent(BUFFER_DETACH_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_probe                        */
/*----------------------------------------------------------*/
void mpi_probe(int * src, int * tag, int * comm, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_probe(src, tag, comm, status, info);
   WTIME(TB2);

   LogEvent(PROBE_ID, TB1, TB2, *src, -1, -1, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_iprobe                       */
/*----------------------------------------------------------*/
void mpi_iprobe(int * src, int * tag, int * comm, int * flag, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_iprobe(src, tag, comm, flag, status, info);
   WTIME(TB2);

   LogEvent(IPROBE_ID, TB1, TB2, *src, -1, -1, *comm);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_test                         */
/*----------------------------------------------------------*/
void mpi_test(int * request, int * flag, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_test(request, flag, status, info);
   WTIME(TB2);

   LogEvent(TEST_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_testany                      */
/*----------------------------------------------------------*/
void mpi_testany(int * num, int * req, int * indx, int * flag, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_testany(num, req, indx, flag, status, info);
   WTIME(TB2);

   LogEvent(TESTANY_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_testall                      */
/*----------------------------------------------------------*/
void mpi_testall(int * num, int * req, int * flag, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_testall(num, req, flag, status, info);
   WTIME(TB2);

   LogEvent(TESTALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_testsome                     */
/*----------------------------------------------------------*/
void mpi_testsome(int * inum, int * req, int * onum, int * ind, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_testsome(inum, req, onum, ind, status, info);
   WTIME(TB2);

   LogEvent(TESTSOME_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_wait                         */
/*----------------------------------------------------------*/
void mpi_wait(int * request, MPI_Status * status, int * info)
{
   int src;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_wait(request, status, info);
   WTIME(TB2);

   src = status->MPI_SOURCE;

   LogEvent(WAIT_ID, TB1, TB2, src, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_waitany                      */
/*----------------------------------------------------------*/
void mpi_waitany(int * num, int * req, int * indx, MPI_Status * status, int * info)
{
   int src;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_waitany(num, req, indx, status, info);
   WTIME(TB2);

   src = status->MPI_SOURCE;

   LogEvent(WAITANY_ID, TB1, TB2, src, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_waitall                      */
/*----------------------------------------------------------*/
void mpi_waitall(int * num, int * req, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_waitall(num, req, status, info);
   WTIME(TB2);

   LogEvent(WAITALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_waitsome                     */
/*----------------------------------------------------------*/
void mpi_waitsome(int * inum, int * req, int * onum, int * ind, MPI_Status * status, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_waitsome(inum, req, onum, ind, status, info);
   WTIME(TB2);

   LogEvent(WAITSOME_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_start                        */
/*----------------------------------------------------------*/
void mpi_start(int * req, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_start(req, info);
   WTIME(TB2);

   LogEvent(START_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_startall                     */
/*----------------------------------------------------------*/
void mpi_startall(int * num, int * req, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_startall(num, req, info);
   WTIME(TB2);

   LogEvent(STARTALL_ID, TB1, TB2, -1, -1, -1, MPI_COMM_WORLD);
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_bcast                        */
/*----------------------------------------------------------*/
void mpi_bcast(void * data, int * count, int * type, 
               int * root, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_bcast(data, count, type, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(BCAST_ID, TB1, TB2, *root, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpe_ibcast                       */
/*----------------------------------------------------------*/
void mpe_ibcast(void * data, int * count, int * type, 
                int * root, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpe_ibcast(data, count, type, root, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(IBCAST_ID, TB1, TB2, *root, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_barrier                      */
/*----------------------------------------------------------*/
void mpi_barrier(int * comm, int * info)
{
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_barrier(comm, info);
   WTIME(TB2);

   LogEvent(BARRIER_ID, TB1, TB2, -1, -1, -1, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_reduce                       */
/*----------------------------------------------------------*/
void mpi_reduce(void * sbuf, void * rbuf, int * count, int * type, 
                int * op, int * root, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_reduce(sbuf, rbuf, count, type, op, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(REDUCE_ID, TB1, TB2, -1, *root, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpe_ireduce                      */
/*----------------------------------------------------------*/
void mpe_ireduce(void * sbuf, void * rbuf, int * count, int * type, 
                int * op, int * root, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpe_ireduce(sbuf, rbuf, count, type, op, root, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(IREDUCE_ID, TB1, TB2, -1, *root, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_allreduce                    */
/*----------------------------------------------------------*/
void mpi_allreduce(void * sbuf, void * rbuf, int * count, int * type, 
                   int * op, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_allreduce(sbuf, rbuf, count, type, op, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(ALLREDUCE_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpe_iallreduce                   */
/*----------------------------------------------------------*/
void mpe_iallreduce(void * sbuf, void * rbuf, int * count, int * type, 
                    int * op, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpe_iallreduce(sbuf, rbuf, count, type, op, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(ALLREDUCE_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_reduce_scatter               */
/*----------------------------------------------------------*/
void mpi_reduce_scatter(void * sbuf, void * rbuf, int * counts, int * type, 
                        int * op, int * comm, int * info)
{
   int i, bytes, num, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_reduce_scatter(sbuf, rbuf, counts, type, op, comm, info);
   WTIME(TB2);

   PMPI_Comm_size(*comm, &tasks);
   PMPI_Type_size(*type, &bytes);

   num = 0;
   for (i=0; i<tasks; i++) num += counts[i];
   bytes = num * bytes;

   LogEvent(REDUCE_SCATTER_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_gather                       */
/*----------------------------------------------------------*/
void mpi_gather(void * sbuf, int * scount, int * stype,
                void * rbuf, int * rcount, int * rtype,
                int * root, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_gather(sbuf, scount, stype, rbuf, rcount, rtype, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*stype, &bytes);
   bytes = (*scount) * bytes;

   LogEvent(GATHER_ID, TB1, TB2, -1, *root, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_gatherv                      */
/*----------------------------------------------------------*/
void mpi_gatherv(void * sbuf, int * scount, int * stype,
                 void * rbuf, int * rcounts, int * rdisp, int * rtype,
                 int * root, int * comm, int * info)
{
   int bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_gatherv(sbuf, scount, stype, rbuf, rcounts, rdisp, rtype, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*stype, &bytes);
   bytes = (*scount) * bytes;

   LogEvent(GATHERV_ID, TB1, TB2, -1, *root, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_scan                         */
/*----------------------------------------------------------*/
void mpi_scan(void * sbuf, void * rbuf, int * count, int * type, 
              int * op, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_scan(sbuf, rbuf, count, type, op, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*type, &bytes);
   bytes = (*count) * bytes;

   LogEvent(SCAN_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_allgather                    */
/*----------------------------------------------------------*/
void mpi_allgather(void * sbuf, int * scount, int * stype,
                   void * rbuf, int * rcount, int * rtype,
                   int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_allgather(sbuf, scount, stype, rbuf, rcount, rtype, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*rtype, &bytes);
   bytes = (*rcount) * bytes;

   LogEvent(ALLGATHER_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_allgatherv                   */
/*----------------------------------------------------------*/
void mpi_allgatherv(void * sbuf, int * scount, int * stype,
                    void * rbuf, int * rcounts, int * rdisp, int * rtype,
                    int * comm, int * info)
{
   int bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_allgatherv(sbuf, scount, stype, rbuf, rcounts, rdisp, rtype, comm, info);
   WTIME(TB2);

   PMPI_Comm_rank(*comm, &id);
   PMPI_Type_size(*rtype, &bytes);
   bytes = rcounts[id] * bytes;

   LogEvent(ALLGATHERV_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_scatter                      */
/*----------------------------------------------------------*/
void mpi_scatter(void * sbuf, int * scount, int * stype,
                 void * rbuf, int * rcount, int * rtype,
                 int * root, int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_scatter(sbuf, scount, stype, rbuf, rcount, rtype, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*rtype, &bytes);
   bytes = (*rcount) * bytes;

   LogEvent(SCATTER_ID, TB1, TB2, *root, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_scatterv                     */
/*----------------------------------------------------------*/
void mpi_scatterv(void * sbuf, int * scounts, int * sdisp, int * stype,
                  void * rbuf, int * rcount, int * rtype,
                  int * root, int * comm, int * info)
{
   int bytes, id;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_scatterv(sbuf, scounts, sdisp, stype, rbuf, rcount, rtype, root, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*rtype, &bytes);
   bytes = (*rcount) * bytes;

   LogEvent(SCATTERV_ID, TB1, TB2, *root, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpe_iscatterv                    */
/*----------------------------------------------------------*/
void mpe_iscatterv(void * sbuf, int * scounts, int * sdisp, int * stype,
                   void * rbuf, int * rcount, int * rtype,
                   int * root, int * comm, int * req, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpe_iscatterv(sbuf, scounts, sdisp, stype, 
                  rbuf, rcount, rtype, root, comm, req, info);
   WTIME(TB2);

   PMPI_Type_size(*rtype, &bytes);
   bytes = (*rcount) * bytes;

   LogEvent(ISCATTERV_ID, TB1, TB2, *root, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_alltoall                     */
/*----------------------------------------------------------*/
void mpi_alltoall(void * sbuf, int * scount, int * stype,
                  void * rbuf, int * rcount, int * rtype,
                  int * comm, int * info)
{
   int bytes;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_alltoall(sbuf, scount, stype, rbuf, rcount, rtype, comm, info);
   WTIME(TB2);

   PMPI_Type_size(*stype, &bytes);
   bytes = (*scount) * bytes;

   LogEvent(ALLTOALL_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_alltoallv                    */
/*----------------------------------------------------------*/
void mpi_alltoallv(void * sbuf, int * scounts, int * sdisp, int * stype,
                   void * rbuf, int * rcounts, int * rdisp, int * rtype,
                   int * comm, int * info)
{
   int i, count, bytes, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpi_alltoallv(sbuf, scounts, sdisp, stype, rbuf, rcounts, rdisp, rtype, comm, info);
   WTIME(TB2);

   PMPI_Comm_size(*comm, &tasks);
   PMPI_Type_size(*stype, &bytes);

   count = 0;
   for (i=0; i<tasks; i++) count += scounts[i];
   bytes = (count * bytes) / tasks;

   LogEvent(ALLTOALLV_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpe_ialltoallv                   */
/*----------------------------------------------------------*/
void mpe_ialltoallv(void * sbuf, int * scounts, int * sdisp, int * stype,
                    void * rbuf, int * rcounts, int * rdisp, int * rtype,
                    int * comm, int * req, int * info)
{
   int i, count, bytes, tasks;
   struct timebasestruct TB1, TB2;

   WTIME(TB1);
   pmpe_ialltoallv(sbuf, scounts, sdisp, stype, 
                   rbuf, rcounts, rdisp, rtype, comm, req, info);
   WTIME(TB2);

   PMPI_Comm_size(*comm, &tasks);
   PMPI_Type_size(*stype, &bytes);

   count = 0;
   for (i=0; i<tasks; i++) count += scounts[i];
   bytes = (count * bytes) / tasks;

   LogEvent(IALLTOALLV_ID, TB1, TB2, -1, -1, bytes, *comm); 
   return;
}

/*----------------------------------------------------------*/
/*    wrapper for Fortran: mpi_finalize                     */
/*----------------------------------------------------------*/
void mpi_finalize(int * info)
{
#include "finalize.c"

   rc = PMPI_Finalize();

  *info = rc;

   return;
}
