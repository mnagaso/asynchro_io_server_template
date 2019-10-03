subroutine do_work()
  use mpi
  implicit none

  ! here is the place where computes

end subroutine


subroutine do_io(inter_comm, ioserve_comm, it)
  use mpi
  implicit none
  integer :: i, num_compute_tasks=4
  integer, intent(in) :: it
  integer, dimension(4) :: array_temp
  integer, dimension(4,4) :: array_glob


  ! mpi vals
  integer, intent(in) :: inter_comm, ioserve_comm
  integer :: ierr, recv_rank
  integer :: status(MPI_STATUS_SIZE)

  array_glob(:,:) = 0
  print *, "state array_glob before do io at it = ", it
  print *, array_glob


  ! Check to see if any data has arrived, if not, then wait.
  ! The probe function will proceed as soon as the first message is ready
  call mpi_probe( MPI_ANY_SOURCE, MPI_ANY_TAG, inter_comm, status, ierr)
  ! Use netCDF/HDF5/ADIOS API to create a file, file variables, and metadata
  !status = ParallelFileCreate( ioserve_comm, . ) 
  
  ! Loop over the number of compute tasks that I have to get messages from
  do i = 1, num_compute_tasks
    call mpi_recv( array_temp, 4, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, inter_comm, status, ierr)
    ! Get the rank in the compute world that sent the data
    recv_rank = status(MPI_SOURCE)
    print *, "received from: ", recv_rank, " array: ", array_temp
    array_glob(:,recv_rank+1) = array_temp
  end do

  print *, "final state of array_glob after do io at it = ", it
  print *, array_glob

  return
end subroutine do_io