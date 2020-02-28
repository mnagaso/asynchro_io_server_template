subroutine do_work()
  use mpi
  implicit none

  ! here is the place where computes

end subroutine


subroutine idle_io(inter_comm, ioserve_comm, io_id, num_recv)
  use mpi

  implicit none

  integer, intent(in)             :: io_id ! id of this io node
  integer, intent(in)             :: num_recv ! number of messages that this io node will reveive
  integer                         :: i, num_compute_tasks=4
  integer, dimension(4,2)         :: array_glob
  double precision, dimension(4,2):: array_glob2

  integer :: tag1, tag2
  integer :: req1=9999, req2=9999 ! mpi req, initialize here with strange numbers to check if its initial states or not
  integer :: rec_count = 0

  ! mpi vals
  integer, intent(in) :: inter_comm, ioserve_comm
  integer :: ierr, recv_rank
  integer :: status(MPI_STATUS_SIZE)

  ! tags of mpi messages used for distingish the arrival data
  tag1 = 1111
  tag2 = 2222

  array_glob(:,:) = 0
  array_glob2(:,:) = 0.0

  rec_count = 0 ! explicit initialization necessary at each call
 
  !!!!!!!!!!!!!
  ! idle loop !
  !!!!!!!!!!!!!
  do while (rec_count < num_recv)
    print *, "rec_count", rec_count, "on io node", io_id
    ! wait till all mpi receiving processes in the former iteration are finished
    call wait_receives(req1, req2)
 
    ! Check to see if any data has arrived, if not, then wait.
    ! The probe function will proceed as soon as the first message is ready
    call mpi_probe( MPI_ANY_SOURCE, MPI_ANY_TAG, inter_comm, status, ierr)
    ! Use netCDF/HDF5/ADIOS API to create a file, file variables, and metadata
    !status = ParallelFileCreate( ioserve_comm, . ) 
 
    if (status(MPI_TAG)==tag1) then
      call get_first_array(io_id, inter_comm, status, array_glob, req1)
      rec_count=rec_count+1
    endif
    if (status(MPI_TAG)==tag2) then
      call get_second_array(io_id, inter_comm, status, array_glob2, req2)
      rec_count=rec_count+1
    endif
 
  enddo

  print *, "received array_glob"
  print *, array_glob
  print *, "received array_glob2"
  print *, array_glob2

end subroutine idle_io


subroutine wait_receives(req1, req2)
  use mpi
  implicit none

  integer, intent(in) :: req1, req2
  integer             :: ier

  ! wait for finishing receiving the first array
  if (req1 == 9999 .or. req2 == 9999) then
    ! have nothing to wait
  else
    call mpi_wait(req1, MPI_STATUS_IGNORE, ier) 
    call mpi_wait(req2, MPI_STATUS_IGNORE, ier) 
  endif
end subroutine wait_receives

subroutine get_first_array(io_id,inter_comm,status,array_glob,req)
  use mpi
  implicit none

  integer, intent(in)                    :: io_id
  integer, intent(in)                    :: inter_comm
  integer, intent(inout)                 :: status(MPI_STATUS_SIZE)
  integer, dimension(4,2), intent(inout) :: array_glob
  integer, intent(out)                   :: req
  
  integer               :: status2(MPI_STATUS_SIZE)
  integer               :: ierr, recv_rank, offset

  ! Get the rank in the compute world that sent the data
  recv_rank = status(MPI_SOURCE)
  offset    = -io_id*2

  ! receive the data with asynchronous receive 
  call mpi_irecv(array_glob(:,recv_rank+1+offset), 4, MPI_INTEGER, status(MPI_SOURCE), &
                 status(MPI_TAG), inter_comm, status2, req, ierr)

end subroutine get_first_array

subroutine get_second_array(io_id,inter_comm,status,array_glob,req)
  use mpi
  implicit none

  integer, intent(in)                             :: io_id
  integer, intent(in)                             :: inter_comm
  integer, intent(in)                             :: status(MPI_STATUS_SIZE)
  double precision, dimension(4,2), intent(inout) :: array_glob
  integer, intent(out)                            :: req

  integer :: status2(MPI_STATUS_SIZE)
  integer :: ierr, recv_rank, offset

  ! Get the rank in the compute world that sent the data
  recv_rank = status(MPI_SOURCE)
  offset    = -io_id*2

  ! Loop over the number of compute tasks that I have to get messages from
  call mpi_irecv(array_glob(:,recv_rank+1+offset), 4, MPI_DOUBLE, status(MPI_SOURCE), status(MPI_TAG), inter_comm, status2, ierr)

end subroutine get_second_array
