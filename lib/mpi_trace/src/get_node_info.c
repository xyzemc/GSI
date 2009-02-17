#include <mpi.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/systemcfg.h>

void get_node_info(int * ncpus,
                   int * tasks_per_node,
                   int * local_rank)
{
  int i, rc;
  int ntasks;
  int taskid;
  int tpn;
  char host[80];
  char * names;
  char * ptr;

  *ncpus = _system_configuration.ncpus;

  rc = PMPI_Comm_size(MPI_COMM_WORLD, &ntasks);

  rc = PMPI_Comm_rank(MPI_COMM_WORLD, &taskid);

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
  tpn = 0;

  for (i=0; i<ntasks; i++)
  {
    ptr = names + i*sizeof(host);
    if (strcmp(host, ptr) == 0) 
    {
      if (taskid == i) *local_rank = tpn;
      tpn++;
    }
  }

  *tasks_per_node = tpn;

  return;

}

