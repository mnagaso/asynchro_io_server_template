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
  integer, dimension(4,4) :: array_glob
  double precision, dimension(4,4):: array_glob2

  integer :: rec_count = 0, num_recv = 8

  ! mpi vals
  integer, intent(in) :: inter_comm, ioserve_comm
  integer :: ierr, recv_rank
  integer :: status(MPI_STATUS_SIZE)

  array_glob(:,:) = 0
  array_glob2(:,:) = 0.0

  !print *, "state array_glob before do io at it = ", it
  !print *, array_glob

  rec_count = 0 ! explicit initialization necessary at each call

  do while (rec_count < num_recv)
  ! Check to see if any data has arrived, if not, then wait.
  ! The probe function will proceed as soon as the first message is ready
    call mpi_probe( MPI_ANY_SOURCE, MPI_ANY_TAG, inter_comm, status, ierr)
    ! Use netCDF/HDF5/ADIOS API to create a file, file variables, and metadata
    !status = ParallelFileCreate( ioserve_comm, . ) 

    if (status(MPI_TAG)==0) then
      call get_first_array(inter_comm,status, array_glob)
      rec_count=rec_count+1
    endif
    if (status(MPI_TAG)==1) then
      call get_second_array(inter_comm,status, array_glob2)
      rec_count=rec_count+1
    endif
  enddo

  print *, "final state of array_glob after do io at it = ", it
  print *, array_glob
  print *, "final state of array_glob2 after do io at it = ", it
  print *, array_glob2

  return
end subroutine do_io

subroutine get_first_array(inter_comm,status, array_glob)
  use mpi
  implicit none

  integer, dimension(4) :: array_temp
  integer, dimension(4,4), intent(inout) :: array_glob
  integer, intent(inout) :: status(MPI_STATUS_SIZE)
  integer :: status2(MPI_STATUS_SIZE)
  integer :: ierr, recv_rank
  integer, intent(in) :: inter_comm

  ! Loop over the number of compute tasks that I have to get messages from
  call mpi_recv( array_temp, 4, MPI_INTEGER, status(MPI_SOURCE), status(MPI_TAG), inter_comm, status2, ierr)
  ! Get the rank in the compute world that sent the data
  recv_rank = status(MPI_SOURCE)
  array_glob(:,recv_rank+1) = array_temp

  print *, "geting local array1 ", array_temp

end subroutine get_first_array

subroutine get_second_array(inter_comm,status,array_glob)
  use mpi
  implicit none

  double precision, dimension(4) :: array_temp
  double precision, dimension(4,4), intent(inout) :: array_glob
  integer, intent(in) :: status(MPI_STATUS_SIZE)
  integer :: status2(MPI_STATUS_SIZE)
  integer :: ierr, recv_rank
  integer, intent(in) :: inter_comm

  ! Loop over the number of compute tasks that I have to get messages from
  call mpi_recv( array_temp, 4, MPI_DOUBLE, status(MPI_SOURCE), status(MPI_TAG), inter_comm, status2, ierr)
  ! Get the rank in the compute world that sent the data
  recv_rank = status(MPI_SOURCE)
  array_glob(:,recv_rank+1) = array_temp


  print *, "geting local array2 ", array_temp


end subroutine get_second_array
