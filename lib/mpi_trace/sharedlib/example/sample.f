program sample
  implicit none
  include 'mpif.h'
  integer i, n, ntasks, taskid, info
  real(8) local_sum, global_sum

  call mpi_init(info)
  call mpi_comm_rank(mpi_comm_world, taskid, info)
  call mpi_comm_size(mpi_comm_world, ntasks, info)

  n = 100000 * ntasks

  local_sum = 0.0d0
  do i = taskid, n, ntasks
    local_sum = local_sum + sqrt(dble(i))
  end do

  call mpi_allreduce(local_sum, global_sum, 1, mpi_real8, mpi_sum, mpi_comm_world, info)

  if (taskid .eq. 0) write(6,*) 'global_sum = ', global_sum

  call mpi_finalize(info)
end
